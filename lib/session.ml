(* slots and lenses *)
type empty = Empty
type all_empty = empty * 'a as 'a

type ('a,'b,'ss,'tt) slot = ('ss -> 'a) * ('ss -> 'b -> 'tt)

let _0 = (fun (a,_) -> a), (fun (_,ss) b -> (b,ss))
let _1 = (fun (_,(a,_)) -> a), (fun (s0,(_,ss)) b -> (s0,(b,ss)))
let _2 = (fun (_,(_,(a,_))) -> a), (fun (s0,(s1,(_,ss))) b -> (s0,(s1,(b,ss))))
let _3 = (fun (_,(_,(_,(a,_)))) -> a), (fun (s0,(s1,(s2,(_,ss)))) b -> (s0,(s1,(s2,(b,ss)))))

let rec all_empty = Empty, all_empty
let _run_internal a f x = snd (f x a)
let run f x = _run_internal all_empty f x
let run_ m = _run_internal all_empty (fun () -> m) ()
           
(* monads *)
type ('ss,'tt,'v) monad = 'ss -> 'tt * 'v

let return x p = p, x
let (>>=) m f p = let q,v = m p in f v q
let (>>) m n p = let q,_ = m p in n q

(* polarized session types *)
type req = Req
type resp = Resp

type cli = req * resp
type serv = resp * req

type 'p cont =
  Msg : ('v * 'p cont Channel.t) -> [`msg of 'r * 'v * 'p] cont
| Branch : 'br  -> [`branch of 'r * 'br] cont
| Chan : (('pp, 'rr) sess * 'p cont Channel.t) -> [`deleg of 'r * ('pp, 'rr) sess * 'p] cont
and ('p, 'r) sess = 'p cont Channel.t * 'r

(* service channels *)
type 'p channel = 'p cont Channel.t Channel.t
let new_channel = Channel.create


module SessionN = struct
  let new_channel = Channel.create

  let close (get,set) ss = set ss Empty, ()

  let send (get,set) v ss =
    let ch,q = get ss and ch' = Channel.create () in
    Channel.send ch (Msg(v,ch')); set ss (ch',q), ()

  let recv (get,set) ss =
    let (ch,q) = get ss in let Msg(v,ch') = Channel.receive ch in
    set ss (ch',q), v

  let deleg_send (get0,set0) ~release:(get1,set1) ss =
    let ch0,q1 = get0 ss and ch0' = Channel.create () in
    let tt = set0 ss (ch0',q1) in
    let ch1,q2 = get1 tt in
    Channel.send ch0 (Chan((ch1,q2),ch0'));
    set1 tt Empty, ()

  let deleg_recv (get0,set0) ~bindto:(get1,set1) ss =
    let ch0,q0 = get0 ss in
    let Chan((ch1',q1),ch0') = Channel.receive ch0 in
    let tt = set0 ss (ch0',q0) in
    set1 tt (ch1',q1), ()

  let accept ch ~bindto:(_,set) ss =
    let ch' = Channel.receive ch in set ss (ch',(Resp,Req)), ()

  let connect ch ~bindto:(_,set) ss =
    let ch' = Channel.create () in Channel.send ch ch'; set ss (ch',(Req,Resp)), ()


  let inp : 'p 'r. 'p -> 'r -> ('p,'r) sess = fun x _ -> Obj.magic x
  let out : 'p 'r. ('p,'r) sess -> 'p = Obj.magic

  let _branch_start : type br.
                           (([`branch of 'r2 * br], 'r1*'r2) sess, 'x, 'ss, 'xx) slot
                           -> (br * ('r1*'r2) -> ('ss, 'uu,'v) monad)
                           -> ('ss, 'uu, 'v) monad
    = fun (get0,set0) f ss ->
    let (ch,p) = get0 ss in
    match Channel.receive ch with
    | Branch(br) -> f (br,p) ss

  let _branch :
        (([`branch of 'r2 * 'br], 'r1*'r2) sess, ('p,'r1*'r2) sess, 'ss, 'tt1) slot
        -> 'p * ('r1*'r2)
        -> ('tt1,'uu,'v) monad
        -> ('ss, 'uu, 'v) monad
    = fun (_,set1) (c,p) m ss ->
    let tt1 = set1 ss (inp c p)
    in m tt1


  let _select : type br p.
                     (([`branch of 'r1 * br],'r1*'r2) sess, (p,'r1*'r2) sess, 'ss, 'tt) slot
                     -> (p -> br)
                     -> ('ss, 'tt, 'v) monad = fun (get0,set0) f ss ->
    let ch,p = get0 ss in
    let k = Channel.create () in
    Channel.send ch (Branch(f (out (k,p))));
    set0 ss (k,p), ()

  let branch2 (s1,f1) (s2,f2) =
    _branch_start s1 (function
                      | `left(p1),q -> _branch s1 (p1,q) (f1 ())
                      | `right(p2),q -> _branch s2 (p2,q) (f2 ())
                     : [`left of 'p1 | `right of 'p2] * 'x -> 'y)

  let select_left s = _select s (fun x -> `left(x))

  let select_right s = _select s (fun x -> `right(x))
end

module Session0 = struct
  let accept_ ch f v = run (fun v -> SessionN.accept ch ~bindto:_0 >> f v) v
  let connect_ ch f v = run (fun v -> SessionN.connect ch ~bindto:_0 >> f v) v

  let close () = SessionN.close _0
  let recv () = SessionN.recv _0
  let send v = SessionN.send _0 v

  let branch2 = fun f g -> SessionN.branch2 (_0,f) (_0,g)
  let select_left () = SessionN.select_left _0
  let select_right () = SessionN.select_right _0

  let _select f = SessionN._select _0 f
  let _branch_start f = SessionN._branch_start _0 f
  let _branch wit m = SessionN._branch _0 wit m

end

type 'a parse_result =
  [`Partial of (string option -> 'a parse_result)
  |`Done    of 'a * string
  |`Fail of string]

module type Adapter = sig
  type raw_chan
  type 'p net = raw_chan -> (('p, serv) sess * all_empty, all_empty, unit) monad
  val req : ('v -> string) -> 'p net -> [`msg of req * 'v * 'p] net
  val resp : (string -> 'v parse_result) -> 'p net -> [`msg of resp * 'v * 'p] net
  val sel : left:'p1 net -> right:'p2 net ->
          [`branch of req * [`left of 'p1|`right of 'p2]] net
  val bra : left:((string -> 'v1 parse_result) * 'p1 net) -> right:'p2 net ->
          [`branch of resp * [`left of [`msg of resp * 'v1 * 'p1] |`right of 'p2]] net
  val cls : [`close] net
end

module Syntax = struct
  let (>>=) = (>>=)
  module Session0 = struct
    let _select = Session0._select
    let _branch_start = Session0._branch_start
    let _branch = Session0._branch
  end
  module SessionN = struct
    let _select = SessionN._select
    let _branch_start = SessionN._branch_start
    let _branch = SessionN._branch
  end
end
