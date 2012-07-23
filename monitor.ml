module type Param = sig 
  type 'a data
  type 'a state
  val create : 'a data -> 'a state
  val destruct : 'a state -> 'a data
end


module Make = functor (X:Param) -> struct
  module M = Mutex
  module C = Condition

  type 'a t = ('a X.state * M.t * C.t)
  
  type 'a wait = WaitMore | Return of 'a
  
  let create v = (X.create v, M.create (), C.create ())

  let wait (cell,m,c) (func: 'a X.state -> 'b wait) : 'b = 
    Mutex.lock m;
    let rec loop () =
      begin try
        match func cell with
        | Return v -> v
        | WaitMore -> begin
            Condition.wait c m; 
            loop ()
          end
      with e ->
        Mutex.unlock m;
        raise e
      end
    in
    let v = loop () in
    Mutex.unlock m;
    v
  
  let signal (_,_,c) = Condition.signal c

  let lock (cell,m,_) (func: 'a X.state -> 'b) : 'b =
    Mutex.lock m;
    begin try
      let v = func cell in
      Mutex.unlock m;
      v
    with e ->
      Mutex.unlock m;
      raise e
    end
  
  let try_lock (cell,m,_) (func: 'a X.state -> 'b) (iffail:'b) : 'b =
    if Mutex.try_lock m then begin
      begin try
        let v = func cell in
        Mutex.unlock m;
        v
      with e ->
        Mutex.unlock m;
        raise e
      end
    end else iffail
  
  let get (cell,_,_) = X.destruct cell
end
