(* slots and lenses *)
type empty = Empty
type all_empty = empty * 'a as 'a
let rec all_empty = Empty, all_empty

type ('a,'b,'pre,'post) slot = {get:('pre -> 'a); put:('pre -> 'b -> 'post)}

let s = {get=(fun (a,_) -> a); put=(fun (_,ss) b -> (b,ss))}
let _0 = {get=(fun (a,_) -> a); put=(fun (_,ss) b -> (b,ss))}
let _1 = {get=(fun (_,(a,_)) -> a); put=(fun (s0,(_,ss)) b -> (s0,(b,ss)))}
let _2 = {get=(fun (_,(_,(a,_))) -> a); put=(fun (s0,(s1,(_,ss))) b -> (s0,(s1,(b,ss))))}
let _3 = {get=(fun (_,(_,(_,(a,_)))) -> a); put=(fun (s0,(s1,(s2,(_,ss)))) b -> (s0,(s1,(s2,(b,ss)))))}
let _4 = {get=(fun (_,(_,(_,(_,(a,_))))) -> a); put=(fun (s0,(s1,(s2,(s3,(_,ss))))) b -> (s0,(s1,(s2,(s3,(b,ss))))))}

let _run_internal a f x = snd (f x a)
let run f x = snd (f x all_empty)
let run_ m = snd (m all_empty)

(* sessions *)
type ('pre,'post,'a) session = 'pre -> 'post * 'a

let return x = fun pre -> pre, x
let (>>=) m f = fun pre -> let mid,a = m pre in f a mid
let (>>) m n = fun pre -> let mid,_ = m pre in n mid

(* polarized session types *)
type req = Req
type resp = Resp

type cli = req * resp
type serv = resp * req

type 'p wrap =
  Msg : ('v * 'p wrap Channel.t) -> [`msg of 'r * 'v * 'p] wrap
| Branch : 'br  -> [`branch of 'r * 'br] wrap
| Chan : (('pp, 'rr) sess * 'p wrap Channel.t) -> [`deleg of 'r * ('pp, 'rr) sess * 'p] wrap
and ('p, 'q) sess = 'p wrap Channel.t * 'q

(* service channels *)
type 'p channel = 'p wrap Channel.t Channel.t
let new_channel = Channel.create

let accept_ ch f x =
  let ch' = Channel.receive ch in
  let ch' = (ch',(Resp,Req)) in
  _run_internal (ch',all_empty) f x

let connect_ ch f x =
  let ch' = Channel.create () in
  Channel.send ch ch';
  let ch' = (ch',(Req,Resp)) in
  _run_internal (ch',all_empty) f x

let close {get;put} = fun pre ->
  put pre Empty, ()

let send {get;put} v = fun pre ->
  let ch,q = get pre
  and ch' = Channel.create () in
  Channel.send ch (Msg(v,ch'));
  put pre (ch',q), ()

let recv {get;put} = fun pre ->
  let (ch,q) = get pre in
  let Msg(v,ch') = Channel.receive ch in
  put pre (ch',q), v

let deleg_send {get=get0;put=put0} ~release:{get=get1;put=put1} = fun pre ->
  let ch0,q1 = get0 pre
  and ch0' = Channel.create () in
  let mid = put0 pre (ch0',q1) in
  let ch1,q2 = get1 mid in
  Channel.send ch0 (Chan((ch1,q2),ch0'));
  put1 mid Empty, ()

let deleg_recv {get=get0;put=put0} ~bindto:{get=get1;put=put1} = fun pre ->
  let ch0,q0 = get0 pre in
  let Chan((ch1',q1),ch0') = Channel.receive ch0 in
  let mid = put0 pre (ch0',q0) in
  put1 mid (ch1',q1), ()

let accept ch ~bindto:{put=set;_} = fun pre ->
  let ch' = Channel.receive ch in
  set pre (ch',(Resp,Req)), ()

let connect ch ~bindto:{put=set;_} = fun pre ->
  let ch' = Channel.create () in
  Channel.send ch ch';
  set pre (ch',(Req,Resp)), ()


let inp : 'p. 'p -> 'p wrap Channel.t = Obj.magic
let out : 'p. 'p wrap Channel.t -> 'p = Obj.magic

let _branch : type br.
                         (([`branch of 'r2 * br], 'r1*'r2) sess, empty, 'pre, 'mid) slot
                         -> (br * ('r1*'r2) -> ('mid, 'post,'v) session)
                         -> ('pre, 'post, 'v) session
  = fun {get=get0;put=set0} f pre ->
  let (ch,p) = get0 pre in
  let mid = set0 pre Empty in
  match Channel.receive ch with
  | Branch(br) -> f (br,p) mid

let _set_sess :
      (empty, ('p,'r1*'r2) sess, 'pre, 'mid) slot
      -> 'p * ('r1*'r2)
      -> ('mid,'post,'v) session
      -> ('pre, 'post, 'v) session
  = fun {put=set1;_} (c,p) m ss ->
  let tt1 = set1 ss ((inp c), p)
  in m tt1


let _select : type br p.
                   (([`branch of 'r1 * br],'r1*'r2) sess, (p,'r1*'r2) sess, 'ss, 'tt) slot
                   -> (p -> br)
                   -> ('ss, 'tt, 'v) session = fun {get=get0;put=set0} f ss ->
  let ch,p = get0 ss in
  let k = Channel.create () in
  Channel.send ch (Branch(f (out k)));
  set0 ss (k,p), ()

let branch ~left:({get=get1;put=put1},f1) ~right:({get=get2;put=put2},f2) = fun pre ->
  let (ch,p) = get1 pre in
  match Channel.receive ch with
  | Branch(`left(ch')) -> f1 () (put1 pre (inp ch', p))
  | Branch(`right(ch')) -> f2 () (put2 pre (inp ch', p))

let select_left s = _select s (fun x -> `left(x))

let select_right s = _select s (fun x -> `right(x))

type 'a parse_result =
  [`Partial of (string option -> 'a parse_result)
  |`Done    of 'a * string
  |`Fail of string]

module type Adapter = sig
  type raw_chan
  type 'p net = raw_chan -> (('p, serv) sess * all_empty, all_empty, unit) session
  val req : ('v -> string) -> 'p net -> [`msg of req * 'v * 'p] net
  val resp : (string -> 'v parse_result) -> 'p net -> [`msg of resp * 'v * 'p] net
  val sel : left:'p1 net -> right:'p2 net ->
          [`branch of req * [`left of 'p1|`right of 'p2]] net
  val bra : left:((string -> 'v1 parse_result) * 'p1 net) -> right:'p2 net ->
          [`branch of resp * [`left of [`msg of resp * 'v1 * 'p1] |`right of 'p2]] net
  val cls : [`close] net
end

module Syntax = struct
  let bind = (>>=)
  let _select = _select
  let _branch = _branch
  let _set_sess = _set_sess
end
