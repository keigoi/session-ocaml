type 'a data = W of 'a
type ('pre, 'post, 'a) lmonad = 'pre -> 'post * 'a
type 'f lbind = 'f

type ('a,'b,'ss,'tt) slot = ('ss -> 'a) * ('ss -> 'b -> 'tt)

let _0 = (fun (a,_) -> a), (fun (_,ss) b -> (b,ss))
let _1 = (fun (_,(a,_)) -> a), (fun (s0,(_,ss)) b -> (s0,(b,ss)))
let _2 = (fun (_,(_,(a,_))) -> a), (fun (s0,(s1,(_,ss))) b -> (s0,(s1,(b,ss))))
let _3 = (fun (_,(_,(_,(a,_)))) -> a), (fun (s0,(s1,(s2,(_,ss)))) b -> (s0,(s1,(s2,(b,ss)))))

type empty = Empty
type all_empty = empty * 'a as 'a

let return a pre = pre, a
let (>>=) f g pre = let mid, la = f pre in g la mid
let (>>) f g pre = let mid, _ = f pre in g mid

module type UnsafeChannel = sig
  type t
  val create     : unit -> t
  val send       : t -> 'a -> unit
  val receive    : t -> 'a
  val reverse    : t -> t
end

type req and resp
type cli = req * resp and serv = resp * req

module type S = sig
  type 'p channel
  val new_channel : unit -> 'p channel

  type ('p,'q) sess

  val accept : 'p channel -> ('pre, 'pre, ('p, serv) sess) lmonad

  val connect : 'p channel -> ('pre, 'pre, ('p, cli) sess) lmonad

  val close : (([`close], 'r1*'r2) sess, empty, 'pre, 'post) slot
              -> ('pre, 'post, unit) lmonad

  val send : 'v -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot
             -> ('pre, 'post, ('p, 'r1*'r2) sess) lmonad

  val receive : (([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot
                -> ('pre, 'post, 'v data * ('p, 'r1*'r2) sess) lmonad

  val select : (('p,'r2*'r1) sess -> ([>] as 'br))
               -> (([`branch of 'r1 * 'br],'r1*'r2) sess, empty, 'pre, 'post) slot
               -> ('pre, 'post, ('p,'r1*'r2) sess) lmonad

  val branch : (([`branch of 'r2 * [>] as 'br], 'r1*'r2) sess, empty, 'pre, 'post) slot
               -> ('pre, 'post, 'br) lmonad

  val deleg_send : (('pp, 'qq) sess, empty, 'mid, 'post) slot
                   -> (([`deleg of 'r1 * ('pp, 'qq) sess * 'p], 'r1*'r2) sess, empty, 'pre, 'mid) slot
                   -> ('pre, 'post, ('p, 'r1*'r2) sess) lmonad

  val deleg_recv :
    (([`deleg of 'r2 * ('pp, 'qq) sess * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot
    -> ('pre, 'post, ('pp,'qq) sess * ('p,'r1*'r2) sess) lmonad

end

module Make(U : UnsafeChannel) : S = struct
  type ('p,'q) sess = U.t
  type 'p channel = U.t Schannel.t

  let new_channel = Schannel.create

  let accept : 'p 'pre. 'p channel -> ('pre, 'pre, ('p, serv) sess) lmonad =
    fun ch pre ->
    let s = Schannel.receive ch in
    pre, s

  let connect : 'p 'pre. 'p channel -> ('pre, 'pre, ('p, cli) sess) lmonad =
    fun ch pre ->
    let s = U.create () in
    Schannel.send ch (U.reverse s);
    pre, s
    
  let close : 'pre 'r1 'r2 'post.
              (([`close], 'r1*'r2) sess, empty, 'pre, 'post) slot
              -> ('pre, 'post, unit) lmonad =
    fun (_,set) pre ->
    set pre Empty, ()

  let send : 'v 'r1 'p 'r2 'pre 'post.
             'v -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot
             -> ('pre, 'post, ('p, 'r1*'r2) sess) lmonad =
    fun v (get,set) pre ->
    let s = get pre in
    U.send s v;
    set pre Empty, s

  let receive : 'r2 'v 'p 'r1 'pre 'post.
                (([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot
                -> ('pre, 'post, 'v data * ('p, 'r1*'r2) sess) lmonad =
    fun (get,set) pre ->
    let s = get pre in
    set pre Empty, (W (U.receive s), s)

  let select : 'p 'r2 'r1 'pre 'post.
               (('p,'r2*'r1) sess -> ([>] as 'br))
               -> (([`branch of 'r1 * 'br],'r1*'r2) sess, empty, 'pre, 'post) slot
               -> ('pre, 'post, ('p,'r1*'r2) sess) lmonad =
    fun f (get,set) pre ->
    let s = get pre in
    U.send s (f (U.reverse s));
    set pre Empty, s

  let branch : 'r2 'r1 'pre 'post.
               (([`branch of 'r2 * [>] as 'br], 'r1*'r2) sess, empty, 'pre, 'post) slot
               -> ('pre, 'post, 'br) lmonad =
    fun (get,set) pre ->
    let s = get pre in
    set pre Empty, (U.receive s)

  let deleg_send : 'pp 'qq 'mid 'post 'r1 'r2 'pre.
                   (('pp, 'qq) sess, empty, 'mid, 'post) slot
                   -> (([`deleg of 'r1 * ('pp, 'qq) sess * 'p], 'r1*'r2) sess, empty, 'pre, 'mid) slot
                   -> ('pre, 'post, ('p, 'r1*'r2) sess) lmonad =
    fun (get1,set1) (get2,set2) pre ->
    let s = get2 pre in
    let mid = set2 pre Empty in
    let t = get1 mid in
    U.send s t;
    set1 mid Empty, s

  let deleg_recv : 'r2 'p 'r1 'pre 'post 'pp 'qq.
                   (([`deleg of 'r2 * ('pp, 'qq) sess * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot
                   -> ('pre, 'post, ('pp,'qq) sess * ('p,'r1*'r2) sess) lmonad =
    fun (get,set) pre ->
    let s = get pre in
    let t = U.receive s in
    set pre Empty, (t, s)    

end
                               
include Make(struct
  type t      = unit Event.channel
  let create     = Event.new_channel
  let send ch x  = Event.sync (Event.send ch (Obj.magic x))
  let receive ch = Obj.magic (Event.sync (Event.receive ch))
  let reverse ch = ch
end)

module Syntax = struct
  let bind = (>>=)

  module Internal = struct
    let __return_raw v pre = pre, v
    let __bind_raw = fun m f pre -> match m pre with (mid,x) -> f x mid

    let __putval_raw = fun (_,set) v pre ->
      set pre v, ()

    let __takeval_raw (get,set) pre =
      set pre Empty, get pre

    let __mkbindfun f = f
    let __dispose_env m pre =
      (>>=) (m pre) (fun (_,a) -> return (Empty, a))
  end
end
