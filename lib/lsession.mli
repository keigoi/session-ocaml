type 'a lin = L of 'a
type 'a data = W of 'a
type ('pre, 'post, 'a) lmonad
type 'f lbind

type ('a,'b,'ss,'tt) slot = ('ss -> 'a) * ('ss -> 'b -> 'tt)
val _0 : ('a, 'b, ('a * 'ss), ('b * 'ss)) slot
val _1 : ('a, 'b, ('s0 * ('a * 'ss)), ('s0 * ('b * 'ss))) slot
val _2 : ('a, 'b, ('s0 * ('s1 * ('a * 'ss))), ('s0 * ('s1 * ('b * 'ss)))) slot
val _3 : ('a, 'b, ('s0 * ('s1 * ('s2 * ('a * 'ss)))), ('s0 * ('s1 * ('s2 * ('b * 'ss))))) slot

type empty
type all_empty = empty * 'a as 'a

val return : 'a -> ('pre, 'pre, 'a lin) lmonad
val (>>=) : ('pre, 'mid, 'a lin) lmonad
  -> ('a lin -> ('mid, 'post, 'b lin) lmonad) lbind
  -> ('pre, 'post, 'b lin) lmonad
val (>>) : ('pre, 'mid, unit lin) lmonad
  -> ('mid, 'post, 'b lin) lmonad
  -> ('pre, 'post, 'b lin) lmonad

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
  type ('p, 'q) lsess = ('p, 'q) sess lin

  val accept : 'p channel -> ('pre, 'pre, ('p, serv) lsess) lmonad

  val connect : 'p channel -> ('pre, 'pre, ('p, cli) lsess) lmonad

  val close : (([`close], 'r1*'r2) lsess, empty, 'pre, 'post) slot
              -> ('pre, 'post, unit lin) lmonad

  val send : 'v -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2) lsess, empty, 'pre, 'post) slot
             -> ('pre, 'post, ('p, 'r1*'r2) lsess) lmonad

  val receive : (([`msg of 'r2 * 'v * 'p], 'r1*'r2) lsess, empty, 'pre, 'post) slot
                -> ('pre, 'post, ('v data * ('p, 'r1*'r2) lsess) lin) lmonad

  val select : (('p,'r2*'r1) lsess -> ([>] as 'br))
               -> (([`branch of 'r1 * 'br],'r1*'r2) lsess, empty, 'pre, 'post) slot
               -> ('pre, 'post, ('p,'r1*'r2) lsess) lmonad

  val branch : (([`branch of 'r2 * [>] as 'br], 'r1*'r2) lsess, empty, 'pre, 'post) slot
               -> ('pre, 'post, 'br lin) lmonad

  val deleg_send : (('pp, 'qq) lsess, empty, 'mid, 'post) slot
                   -> (([`deleg of 'r1 * ('pp, 'qq) lsess * 'p], 'r1*'r2) lsess, empty, 'pre, 'mid) slot
                   -> ('pre, 'post, ('p, 'r1*'r2) lsess) lmonad

  val deleg_recv :
    (([`deleg of 'r2 * ('pp, 'qq) lsess * 'p], 'r1*'r2) lsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, (('pp,'qq) lsess * ('p,'r1*'r2) lsess) lin) lmonad

end
            
module Make(U : UnsafeChannel) : S
module UnsafeChannel : UnsafeChannel
              
include S

module Syntax : sig
  val bind : ('pre, 'mid, 'a lin) lmonad
             -> ('a lin -> ('mid, 'post, 'b lin) lmonad) lbind
             -> ('pre, 'post, 'b lin) lmonad

  module Internal : sig
    val __bind_raw : ('pre,'mid,'a) lmonad -> ('a -> ('mid,'post,'b) lmonad) -> ('pre,'post,'b) lmonad
    val __return_raw : 'a -> ('p,'p,'a) lmonad

    val __mkbindfun : ('a -> ('pre,'post,'b) lmonad) -> ('a -> ('pre, 'post, 'b) lmonad) lbind

    val __putval_raw : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) lmonad
    val __takeval_raw : ('a lin,empty,'pre,'post) slot -> ('pre,'post,'a) lmonad
  end
end
