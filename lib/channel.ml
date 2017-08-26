module type IO = sig
  type +'a io
  val return : 'a -> 'a io
  val (>>=) : 'a io -> ('a -> 'b io) -> 'b io
end

module type MUTEX = sig
  type +'a io
  type t
  val create : unit -> t
  val lock : t -> unit io
  val unlock : t -> unit
end

module type CONDITION = sig
  type +'a io
  type m
  type t
  val create : unit -> t
  val signal : t -> unit
  val wait : t -> m -> unit io
end

module type S = sig
  type +'a io
  type 'a t
  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit io
  val receive : 'a t -> 'a io
end  

module Make(IO:IO)(M:MUTEX with type 'a io = 'a IO.io)(C:CONDITION with type 'a io = 'a IO.io and type m = M.t)
: S with type 'a io = 'a IO.io
= struct
  open IO
  module Q = Queue

  type +'a io = 'a IO.io
  type 'a t = 'a Q.t * M.t * C.t

  let create () : 'a t = Q.create (), M.create (), C.create ()

  let send (q,m,c) v =
    M.lock m >>= fun _ ->
    Q.add v q;
    C.signal c;
    M.unlock m;
    return ()

  let receive (q,m,c) =
    M.lock m >>= fun _ ->
    let rec loop () =
      if Q.is_empty q then
        C.wait c m >>=
        loop
      else
        return (Q.take q)
    in
    loop () >>= fun v ->
    M.unlock m;
    return v
end
