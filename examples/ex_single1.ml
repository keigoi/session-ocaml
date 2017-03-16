(* Single-session example *)
open Session
open Session0

let neg_server () =
  let%s x = recv () in
  send (-x) >>
  close ()

let neg_client () =
  send 12345 >>
  let%s x = recv () in
  Printf.printf "Negated: %d\n" x; (* prints "-12345" *)
  close ()
        
let neg_ch = new_channel ()
                         
let _ =
  Lwt_main.run @@ Lwt.join [
      accept_ neg_ch neg_server ();
      connect_ neg_ch neg_client ()]

