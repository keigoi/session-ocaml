(* Multi-session example with slots and lenses *)
open Session
open SessionN
open Ex_single2

open Session;;

(* The service channel between the main thread and workers *)
let deleg_ch = new_channel ();;

(* The main thread *)
let rec main_thread () =
  accept arith_ch ~bindto:_0 >> connect deleg_ch ~bindto:_1 >>
  deleg_send _1 ~release:_0 >>
  close _1 >>= main_thread;;

(* The worker thread *)
let rec worker_thread i () =
  let%s _ = accept deleg_ch ~bindto:_1 >> deleg_recv _1 ~bindto:_0 in
  (Printf.printf "worker %d\n" i;
  close _1) >>
  arith_server () >>= worker_thread i;;

(* Invokation of threads *)
for i = 0 to 5 do ignore @@ Thread.create (run @@ worker_thread i) () done;
Thread.create (run main_thread) ();;

for i = 0 to 10 do
  Session0.connect_ arith_ch arith_client ()
done;;
