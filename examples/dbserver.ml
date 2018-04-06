open Session

type result = Result (*stub*)
type credential = Cred (*stub*)
let bad_credential Cred = false (*stub*)
let do_query (query:string) : result = Result (*stub*)
   
let db_ch = new_channel ()
and worker_ch = new_channel ()  
  
let rec main () =
  accept db_ch ~bindto:_0 >>
  recv _0 >>= fun cred ->
  if bad_credential cred then
    select_left _0 >>
    close _0
  else
    select_right _0 >>
    connect worker_ch ~bindto:_1 >>
    deleg_send _1 ~release:_0 >>
    close _1 >>=
    main

let rec worker () =
  accept worker_ch ~bindto:_0 >>
  deleg_recv _0 ~bindto:_1 >>
  close _0 >>
  let rec loop () =
    branch
      ~left:(_1, fun () -> close _1)
      ~right:(_1, fun () ->
          recv _1 >>= fun query ->
          let res = do_query query in
          send _1 res >>=
          loop)
  in loop () >>= worker
