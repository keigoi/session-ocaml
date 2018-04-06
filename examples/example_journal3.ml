open Session
open Example_journal2
let worker_ch = new_channel ()
let rec main () =
  accept log_ch ~bindto:_0 >>
  connect worker_ch ~bindto:_1 >>
  deleg_send _1 ~release:_0 >>
  close _1 >>= fun () ->
  main ()
let rec worker () =
  accept worker_ch ~bindto:_1 >>
  deleg_recv _1 ~bindto:_0 >>
  close _1 >>
  logic_server () >>= fun () ->
  worker ();;
for i = 0 to 5 do
  Thread.create (run worker) ()
done;;
Thread.create (run main) ();;
connect_ log_ch (fun () ->
  select_left s >>
  send s Or >>
  send s (true, false) >>
  recv s >>= fun ans ->
  print_bool ans; print_newline ();
  select_left s >>
  send s And >>
  send s (true, false) >>
  recv s >>= fun ans ->
  print_bool ans; print_newline ();
  select_left s >>
  send s Xor >>
  send s (true, false) >>
  recv s >>= fun ans ->
  print_bool ans; print_newline ();
  select_right s >>
  close s) ()
    
