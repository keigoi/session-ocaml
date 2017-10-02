open Linocaml.Direct
open Session_ocaml.Direct.Net

let get buf pos = Char.code @@ Bytes.get buf pos

let recv_packet tcp =
  let buf = Bytes.create 4 in
  really_input tcp.in_ buf 0 4;
  let code = get buf 3
  and len = get buf 2 lsl 16 + get buf 1 lsl 8 + get buf 0
  in
  really_input tcp.in_ buf "
