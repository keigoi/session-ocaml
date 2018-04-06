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
type empty_three = empty * (empty * (empty * empty))
type empty_four = empty * empty_three

val return : 'a -> ('pre, 'pre, 'a) lmonad
val (>>=) : ('pre, 'mid, 'a) lmonad
  -> ('a -> ('mid, 'post, 'b) lmonad) lbind
  -> ('pre, 'post, 'b) lmonad
val (>>) : ('pre, 'mid, unit) lmonad
  -> ('mid, 'post, 'b) lmonad
  -> ('pre, 'post, 'b) lmonad

val run : (unit -> (empty_four, empty_four, unit) lmonad) -> unit

type req and resp
type cli = req * resp and serv = resp * req

module type S = sig
  type ('p, 'q, 'c) sess
  type ('p, 'q, 'c) dsess = ('p, 'q, 'c) sess lin
  type shmem

  val _mksess : 'c -> ('p, 'q, 'c) dsess

  (* connectors *)
  type ('p,'c) connector
  type ('p,'c) acceptor

  val create_connector : (unit -> 'c) -> ('p,'c) connector
  val create_acceptor : (unit -> 'c) -> ('p,'c) acceptor

  module Sender : sig
    type ('c,'v) t = ('c -> 'v -> unit, [%imp Senders]) Ppx_implicits.t
  end
  module Receiver : sig
    type ('c,'v) t = ('c -> 'v, [%imp Receivers]) Ppx_implicits.t
  end
  module Closer : sig
    type 'c t = ('c -> unit, [%imp Closers]) Ppx_implicits.t
  end

  (* connections on shared memory *)
  val new_shmem_channel : unit -> ('p,shmem) connector * ('p,shmem) acceptor
  module Senders : sig
    val _f : shmem -> 'v -> unit
  end
  module Receivers : sig
    val _f : shmem -> 'v
  end
  module Closers : sig
    val _f : shmem -> unit
  end

  val accept : ('p,'c) acceptor -> ('pre, 'pre, ('p, serv, 'c) dsess) lmonad
    
  val connect : ('p,'c) connector -> ('pre, 'pre, ('p, cli, 'c) dsess) lmonad

  val close :
    ?_closer:'c Closer.t ->
    (([`close], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, unit) lmonad

  val send :
    ?_sender:('c, 'v) Sender.t
    -> 'v -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, ('p, 'r1*'r2, 'c) dsess) lmonad

  val receive :
    ?_receiver:('c, 'v) Receiver.t
    -> (([`msg of 'r2 * 'v * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, 'v data * ('p, 'r1*'r2, 'c) dsess) lmonad

  val select :
    ?_sender:('c, [>] as 'br) Sender.t
    -> (('p,'r2*'r1, 'c) dsess -> 'br)
    -> (([`branch of 'r1 * 'br],'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, ('p,'r1*'r2, 'c) dsess) lmonad

  val branch :
    ?_receiver:('c, [>] as 'br) Receiver.t
    -> (([`branch of 'r2 * 'br], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, 'br) lmonad

  val deleg_send :
    ?_sender:('c , ('pp,'qq,'cc) sess) Sender.t
    -> (('pp, 'qq, 'cc) dsess, empty, 'mid, 'post) slot
    -> (([`deleg of 'r1 * ('pp, 'qq, 'cc) dsess * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'mid) slot
    -> ('pre, 'post, ('p, 'r1*'r2, 'c) dsess) lmonad

  val deleg_recv :
    ?_receiver:('c, ('pp, 'qq, 'cc) sess) Receiver.t
    -> (([`deleg of 'r2 * ('pp, 'qq, 'cc) dsess * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, ('pp,'qq,'cc) dsess * ('p,'r1*'r2,'c) dsess) lmonad

end
            
module Make(U : sig
  type t
  val create     : unit -> t
  val send       : t -> 'a -> unit
  val receive    : t -> 'a
  val reverse    : t -> t
end
) : S
include S

module Syntax : sig
  val bind : ('pre, 'mid, 'a) lmonad
             -> ('a -> ('mid, 'post, 'b) lmonad) lbind
             -> ('pre, 'post, 'b) lmonad

  module Internal : sig
    val __bind_raw : ('pre,'mid,'a) lmonad -> ('a -> ('mid,'post,'b) lmonad) -> ('pre,'post,'b) lmonad
    val __return_raw : 'a -> ('p,'p,'a) lmonad

    val __mkbindfun : ('a -> ('pre,'post,'b) lmonad) -> ('a -> ('pre, 'post, 'b) lmonad) lbind

    val __putval_raw : (empty,'a lin,'pre,'post) slot -> 'a -> ('pre,'post,unit) lmonad
    val __takeval_raw : ('a,empty,'pre,'post) slot -> ('pre,'post,'a) lmonad
  end
end

module Tcp : sig
  type stream = {in_:in_channel; out:out_channel}
  val connector : host:string -> port:int -> ('p, stream) connector
  val new_domain_channel : unit -> ('p, stream) connector * ('p, stream) acceptor
  val fork : (('p, serv, stream) dsess * (empty * (empty * (empty * empty))), empty_four, unit lin) lmonad
             -> (('p, cli, stream) dsess * (empty * (empty * (empty * empty))), empty, 'v lin) lmonad
             -> 'v

  module Closers : sig
    val _f : stream -> unit
  end
end
