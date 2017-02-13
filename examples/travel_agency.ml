open Session
open SessionN

type address = Address of string
type date = Date of int                        

(*
let customer ch =
  connect ch ~bindto:_0 >>
  let rec loop () = 
    [%select _0 `quote(x)] >>
    send _0 "London to Paris, Eurostar" >>
    let%s cost = recv _0 in
    if cost < 100. then
      [%select _0 `accept]
    else
      loop ()
  in
  loop () >>
  send _0 (Address("Kensington, London SW7 2AZ, UK")) >>
  let (Date(date)) = recv _0 in
  close _0             
  *)
   
let customer ch =
  connect ch ~bindto:_0 >>
  let rec loop () = 
    _select _0 (fun x -> `quote(x)) >>
    send _0 "London to Paris, Eurostar" >>
    recv _0 >>= fun cost ->
    if cost < 100. then
      _select _0 (fun x -> `accept(x)) >>
      return cost
    else
      loop ()
  in
  loop () >>= fun cost ->
  send _0 (Address("Kensington, London SW7 2AZ, UK")) >>
  recv _0 >>= fun (Date(date)) ->
  close _0 >>
  (Printf.printf "customer done. cost: %f\n" cost; return ())

(*
val customer :
  ([ `branch of
       Session.req *
       [> `accept of
            [ `msg of
                Session.req * address *
                [ `msg of Session.resp * date * [ `close ] ] ]
        | `quote of
            [ `msg of
                Session.req * string * [ `msg of Session.resp * float * 'a ] ] ] ]
   as 'a)
  Session.channel ->
  (Session.empty * 'b, Session.empty * 'b, unit) Session.monad = <fun>
 *)

(*  
let agency ctm_ch svc_ch  =
  accept ctm_ch ~bindto:_0 >>
  let rec loop () =
    match%branch _0 with
    | `accept(p),q -> return ()
    | `quote(p),q -> begin
         let%s dest = recv _0 in
         send _0 80.00 >>
         loop ()
       end
  in
  loop () >>
  connect svc_ch ~bindto:_1 >>
  deleg_send _1 ~release:_0 >>
  close _1    
 *)
  
let agency ctm_ch svc_ch  =
  accept ctm_ch ~bindto:_0 >>
  let rec loop () =
    _branch_start
      _0 (function
          | `accept(p),q -> _branch _0 (p,q) (return ())
          | `quote(p),q -> _branch _0 (p,q) begin
                                     recv _0 >>= fun dest ->
                                     send _0 80.00 >>
                                     loop ()
                                   end)
  in
  loop () >>
  connect svc_ch ~bindto:_1 >>
  deleg_send _1 ~release:_0 >>
  close _1    

(*
val agency :
  ([ `branch of
       Session.req *
       [< `accept of 'b
        | `quote of
            [ `msg of
                Session.req * 'c * [ `msg of Session.resp * float * 'a ] ] ] ]
   as 'a)
  Session.channel ->
  [ `deleg of
      Session.req * ('b, Session.resp * Session.req) Session.sess *
      [ `close ] ]
  Session.channel ->
  (Session.empty * (Session.empty * 'd),
   Session.empty * (Session.empty * 'd), unit)
  Session.monad = <fun>
 *)

let service svc_ch =
  accept svc_ch ~bindto:_0 >>
  deleg_recv _0 ~bindto:_1 >>
  recv _1 >>= fun (Address(address)) ->
  send _1 (Date(0)) >>
  close _1 >>
  close _0

(*
val service :
  [ `deleg of
      Session.req *
      ([ `msg of 'a * address * [ `msg of 'b * date * [ `close ] ] ],
       'b * 'a)
      Session.sess * [ `close ] ]
  Session.channel ->
  (Session.empty * (Session.empty * 'c),
   Session.empty * (Session.empty * 'c), unit)
  Session.monad = <fun>
 *)

let ctm_ch = new_channel ()
let svc_ch = new_channel ()

let _ =
  Thread.create (run service) svc_ch;
  Thread.create (run (agency ctm_ch)) svc_ch;
  run customer ctm_ch

  
