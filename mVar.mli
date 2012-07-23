(** MVar. see http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html *)
type 'a t
val empty : unit -> 'a t
val create : 'a -> 'a t
val take : 'a t -> 'a
val put : 'a t -> 'a -> unit
val read : 'a t -> 'a
val swap : 'a t -> 'a -> 'a
val try_take : 'a t -> 'a option
val try_put : 'a t -> 'a -> bool
val is_empty : 'a t -> bool
val with_ : 'a t -> ('a -> 'b) -> 'b
val modify_ : 'a t -> ('a -> 'a) -> unit
val modify : 'a t -> ('a -> ('a * 'b)) -> 'b
