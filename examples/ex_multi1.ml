(* Multi-session example with slots and lenses *)
open Session
open SessionN

let rec main_thread dch () =
  accept Ex_single2.arith_ch ~bindto:_0 >>
    connect dch ~bindto:_1 >>
    deleg_send _1 ~release:_0 >>
    close _1 >>=
    main_thread dch

let rec worker_thread dch () =
  accept dch ~bindto:_1 >>
    deleg_recv _1 ~bindto:_0 >>
    close _1 >>
    Ex_single2.arith_server () >>= (* arith_server uses _0 *)
    worker_thread dch

let _ =
  let deleg_ch = new_channel () in
  for i = 0 to 5 do
    ignore @@ Thread.create
      (run (worker_thread deleg_ch)) ()
  done;
  ignore @@ Thread.create (run (main_thread deleg_ch)) ();
  ignore @@ Thread.create (Session0.connect_ Ex_single2.arith_ch Ex_single2.arith_client) ();
  ignore @@ Thread.create (Session0.connect_ Ex_single2.arith_ch Ex_single2.arith_client) ();
  Session0.connect_ Ex_single2.arith_ch Ex_single2.arith_client ();
  Session0.connect_ Ex_single2.arith_ch Ex_single2.arith_client ();
                
  Unix.sleep 1
