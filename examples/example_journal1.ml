open Session
let xor : bool -> bool -> bool = (<>)
let print_bool = Printf.printf "%B"
let xor_ch = new_channel ();;
Thread.create
  (accept_ xor_ch (fun () ->
    recv s >>= fun (x,y) ->
    send s (xor x y) >>
    close s)) ();;
connect_ xor_ch (fun () ->
  send s (false,true) >>
  recv s >>= fun b ->
  print_bool b;
  close s) ()
