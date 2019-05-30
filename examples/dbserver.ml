open Session_ocaml.Session

type result = Result (*stub*)
type credential = Cred (*stub*)
let bad_credential Cred = false (*stub*)
let do_query (query:string) : result = Result (*stub*)

let worker_ch = new_channel ()

let rec main db_ch =
  accept db_ch _0 >>
  recv _0 >>= fun cred ->
  if bad_credential cred then
    select_left _0 >>
    close _0
  else
    select_right _0 >>
    connect worker_ch _1 >>
    deleg_send _1 _0 >>
    close _1 >>= fun () ->
    main db_ch

let rec worker () =
  accept worker_ch _0 >>
  deleg_recv _0 _1 >>
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
