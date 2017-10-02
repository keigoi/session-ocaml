module UnsafeChannel : sig
  type 'a t
  type +'a io = 'a
  val create     : unit -> 'a t
  val send       : 'a t -> 'a -> unit
  val receive    : 'a t -> 'a
end = struct
  type 'a t      = 'a Event.channel
  type +'a io = 'a
  let create     = Event.new_channel
  let send ch x  = Event.sync (Event.send ch x)
  let receive ch = Event.sync (Event.receive ch)
end

include Base.Make(Linocaml.Direct)(UnsafeChannel)



