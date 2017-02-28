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
let rec worker_thread () =
  accept deleg_ch ~bindto:_1 >> deleg_recv _1 ~bindto:_0 >>
  close _1 >>
  arith_server () >>= worker_thread;;

(* Invokation of threads *)
for i = 0 to 5 do ignore @@ Thread.create (run worker_thread) () done;
run main_thread ();;
