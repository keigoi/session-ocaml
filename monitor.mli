(** Monitor in a functorial style. *)

(**
   The parameter type for the monitor. 
   - 'a data is the raw representation type of the data that is hold inside the monitor, and
   - 'a state is some kind of stateful data cell (typically 'a ref),
     which is assured to be accessed by only one thread at one time.
 *)
module type Param =
  sig
    type 'a data
    type 'a state
    val create : 'a data -> 'a state
    val destruct : 'a state -> 'a data
  end

module Make :   
  functor (X : Param) ->
    sig
      type 'a t
      type 'a wait = WaitMore | Return of 'a
      val create : 'a X.data -> 'a t
      val wait : 'a t -> ('a X.state -> 'b wait) -> 'b
      val lock : 'a t -> ('a X.state -> 'b) -> 'b
      val try_lock : 'a t -> ('a X.state -> 'b) -> 'b -> 'b
      val get : 'a t -> 'a X.data
    end
