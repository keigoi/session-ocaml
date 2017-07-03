
type ('a,'b) cxt = <cli:'a; wrk:'b>
[@@deriving lens]

open Session
open SessionN
;;

  
let%w x = recv _0
and y = recv _0
in return ()    
;;

let ch = new_channel ()
;;

let f = 
  let%lin #cli = accept ch in
  return ()
   
