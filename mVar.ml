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
    | Some v -> 
      cell := None; (* full, then make it empty and return *)
      M.signal t; 
      M.Return v)

let put (t:'a t) (v:'a) : unit = 
  M.wait t (fun cell ->
    match !cell with
    | Some _ -> M.WaitMore (* if full, wait further *)
    | None -> 
      cell := Some v; (* empty, then put v and return *)
      M.signal t; 
      M.Return ())

let read t = 
  M.wait t (fun cell -> 
    match !cell with
    | None -> M.WaitMore
    | Some v -> M.Return v)

let swap (t:'a t) (v:'a) : 'a = 
  M.wait t (fun cell ->
    match !cell with
    | None -> M.WaitMore (* wait *)
    | Some w -> 
      cell := Some v; 
      M.signal t;
      M.Return w (*swap and return*))

let try_take (t:'a t) : 'a option =
  M.try_lock t 
    (fun cell -> 
      match !cell with
      | None -> None
      | Some v -> 
        cell := None; 
        M.signal t;
        Some v)
    None

let try_put (t:'a t) (v:'a) : bool =
  M.try_lock t 
    (fun cell ->
      match !cell with
      | Some _ -> false
      | None -> 
        cell := Some v; 
        M.signal t;
        true)
    false

let is_empty (t:'a t) = M.get t=None

let with_ (t:'a t) (f:'a -> 'b) =
  M.wait t (fun cell ->
    match !cell with
    | None -> M.WaitMore
    | Some v -> M.Return (f v))

let modify_ (t:'a t) (f:'a -> 'a) : unit =
  M.wait t (fun cell ->
    match !cell with
    | None -> M.WaitMore
    | Some v -> 
      cell := Some (f v); 
      M.Return ())

let modify (t:'a t) (f:'a -> 'a * 'b) : 'b =
  M.wait t (fun cell ->
    match !cell with
    | None -> M.WaitMore
    | Some v -> 
      let v, w = f v in
      cell := Some v; 
      M.Return w)
