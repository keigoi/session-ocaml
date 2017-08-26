module type S = sig
  type +'a io
  type 'a t

  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit io
  val receive : 'a t -> 'a io
  val reverse : 'a t -> 'a t
end
module Make(Chan:Channel.S)
: S with type 'a io = 'a Chan.io
  = struct
  type +'a io = 'a Chan.io
  type 'a t = 'a Chan.t * 'a Chan.t

  let create () = Chan.create (), Chan.create ()
  let send (w,_) v = Chan.send w v
  let receive (_,r) = Chan.receive r
  let reverse (w,r) = (r,w)
end

(* module type S = sig *)
(*   type +'a io *)
(*   type ('a,'q) t *)
(*   type req and resp *)
(*   val create : unit -> ('a, req*resp) t *)
(*   val send : ('a,'q) t -> 'a -> unit io *)
(*   val receive : ('a,'q) t -> 'a io *)
(*   val reverse : ('a,'r1*'r2) t -> ('a,'r2*'r1) t *)
(* end *)
(* module Make(Chan:Channel.S) *)
(* : S with type 'a io = 'a Chan.io *)
(*   = struct *)
(*   type +'a io = 'a Chan.io *)
(*   type ('a,'q) t = 'a Chan.t * 'a Chan.t *)
(*   type req and resp *)

(*   let create () = Chan.create (), Chan.create () *)
(*   let send (w,_) v = Chan.send w v *)
(*   let receive (_,r) = Chan.receive r *)
(*   let reverse (w,r) = (r,w) *)
(* end *)
