module type RAW_CHAN = sig
  type +'a io
  type t
  val create : unit -> t
  val send : t -> 'a -> unit io
  val receive : t -> 'a io
  val close : t -> unit io
  val reverse : t -> t
end
