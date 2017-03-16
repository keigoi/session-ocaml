type t
val create_empty : unit -> t
val put : t -> 'a -> unit Lwt.t
val take : t -> 'a Lwt.t
