type 'a t
val create     : unit -> 'a t
val send       : 'a t -> 'a -> unit
val receive    : 'a t -> 'a
