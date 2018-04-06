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

val return : 'a -> ('pre, 'pre, 'a) lmonad
val (>>=) : ('pre, 'mid, 'a) lmonad
  -> ('a -> ('mid, 'post, 'b) lmonad) lbind
  -> ('pre, 'post, 'b) lmonad
val (>>) : ('pre, 'mid, unit) lmonad
  -> ('mid, 'post, 'b) lmonad
  -> ('pre, 'post, 'b) lmonad

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
            
module Make(U : UnsafeChannel) : S
              
include S

module Syntax : sig
  val bind : ('pre, 'mid, 'a) lmonad
             -> ('a -> ('mid, 'post, 'b) lmonad) lbind
             -> ('pre, 'post, 'b) lmonad

  module Internal : sig
    val __bind_raw : ('pre,'mid,'a) lmonad -> ('a -> ('mid,'post,'b) lmonad) -> ('pre,'post,'b) lmonad
    val __return_raw : 'a -> ('p,'p,'a) lmonad

    val __mkbindfun : ('a -> ('pre,'post,'b) lmonad) -> ('a -> ('pre, 'post, 'b) lmonad) lbind

    val __putval_raw : (empty,'a,'pre,'post) slot -> 'a -> ('pre,'post,unit) lmonad
    val __takeval_raw : ('a,empty,'pre,'post) slot -> ('pre,'post,'a) lmonad
  end
end
