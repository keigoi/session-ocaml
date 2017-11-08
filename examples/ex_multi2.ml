
open Lsession
;;


let ch = new_channel ()
;;
let s = _0

let f () = 
  let%lin #s = accept ch in
  let%lin #s = send 100 s in
  close s

