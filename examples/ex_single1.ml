(* Single-session example *)
open Session
open Session0

let neg_server () =
  recv () >>= fun x ->
  send (-x) >>
    close ()

let neg_client () =
  send 12345 >>
    recv () >>= fun x ->
  Printf.printf "Negated: %d\n" x; (* prints "-12345" *)
  close ()
        
let neg_ch = new_channel ()
                         
let _ =
  ignore @@ Thread.create
              (accept_ neg_ch neg_server) ();
  connect_ neg_ch neg_client ()

