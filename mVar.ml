module M = Monitor.Make (struct 
    type 'a data = 'a option
    type 'a state = 'a option ref 
    let create v = ref v
    let destruct c = !c
  end)

type 'a t = 'a M.t

let create v = M.create (Some v)

let empty () = M.create None

let take (t:'a t) : 'a = 
  M.wait t (fun cell ->
    match !cell with
    | None -> M.WaitMore (* if empty, wait further *)
    | Some v -> cell := None; M.Return v (* full, then make it empty and return *))

let put (t:'a t) (v:'a) : unit = 
  M.wait t (fun cell ->
    match !cell with
    | Some _ -> M.WaitMore (* if full, wait further *)
    | None -> cell := Some v; M.Return () (* empty, then put v and return *))

let read t = 
  M.wait t (fun cell -> 
    match !cell with
    | None -> M.WaitMore
    | Some v -> M.Return v)

let swap (t:'a t) (v:'a) : 'a = 
  M.wait t (fun cell ->
    match !cell with
    | None -> M.WaitMore (* wait *)
    | Some w -> cell := Some v; M.Return w (*swap and return*))

let try_take (t:'a t) : 'a option =
  M.try_lock t 
    (fun cell -> 
      match !cell with
      | None -> None
      | Some v -> cell := None; Some v)
    None

let try_put (t:'a t) (v:'a) : bool =
  M.try_lock t 
    (fun cell ->
      match !cell with
      | None -> cell := Some v; true
      | Some _ -> false)
    false

let is_empty (t:'a t) = M.get t=None
