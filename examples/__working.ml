(* under reconstruction *)

                     (*
module type LinSession = sig
  type (-'a, +'b) t
  type 'a bang
  val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val fst : ('a -> 'b) t -> ('a * 't, 'b * 't) t
  val snd : ('a -> 'b) t -> ('t * 'a, 't * 'b) t
  val connect : 'p channel -> (unit, ('p, cli) sess) t
  val accept : 'p channel -> (unit, ('p, serv) sess) t
  val bindto : 'v -> (unit, 'v) t
  val out :  ('a -> 'b) -> ('a bang, 
  val send : (([`msg * 'r1 * 'v * 'p], 'r1 * 'r2) sess * 'v bang, ('p, 'r1 * 'r2) sess) t
  val recv : (([`msg * 'r2 * 'v * 'p], 'r1 * 'r2) sess, ('p, 'r1 * 'r2) sess * 'v bang) t
end

module type MonadAlt = sig
  type (-'a, +'b) t
  type ('a, 'b, 's, 't) lens
  val fst : ('a, 'b) t -> ('a * 't, 'b * 't) t
  val snd : ('a, 'b) t -> ('t * 'a, 't * 'b) t
  val send : 'v -> ([`msg of 'r1 * 'v * 'p], 'p) t
  val recv : ([`msg of 'r1 * 'v * 'p], 'p) t

  val lift : ('a,'b,'ss,'tt) lens -> ('a, 'b, 'c) t -> ('ss, 'tt, 'c) t
end

module type MonadAlt = sig
  type (-'a, +'b, +'c) t
  val (>>) : ('a, 'b, unit) t -> ('b, 'c, 'w) t -> ('a, 'c, 'w) t
  val (>>=) : ('a, 'b, 'v) t -> ('v -> ('b, 'c, 'w) t) -> ('a, 'c, 'w) t
  val return : 'v -> ('a, 'a, 'v) t
  val fst : ('a, 'b, 'c) t -> ('a * 't, 'b * 't, 'c) t
  val snd : ('a, 'b, 'c) t -> ('t * 'a, 't * 'b, 'c) t
  val send : 'v -> ([`msg of 'r1 * 'v * 'p], 'p, unit) t
  val recv : unit -> ([`msg of 'r1 * 'v * 'p], 'p, 'v) t
end
module M(X:MonadAlt) = struct
  open X
  let _0 = fst
  let _ = _0 (send 100) >> _1 (recv ()) >> 
end
                     
  
                     
module type TcpSessionS = sig
  type 'p net = Tcp.channel -> (('p, serv) sess * all_empty, all_empty, unit) session
  val req : ('v -> string) -> 'p part -> [`msg of req * 'v * 'p] part
  val resp : (string -> 'v) -> 'p part -> [`msg of resp * 'v * 'p] part
  val sel :
      : (('v1 -> string) * 'p1 part)
        -> (('v2 -> string) * 'p2 part)
        -> [`branch of req * [`left * 'p1 | `right * 'p2]] part
  val bra :
      : ((string -> 'v1 option) * 'p1 part)
        -> ((string -> 'v2 option) * 'p2 part)
        -> [`branch of resp * [`left * [`msg of resp * 'v1 * 'p1]
                              |`right * [`msg of resp * 'v2 * 'p2]]] part
  val cls : [`close] part
end
module TcpSession : TcpSessionS = struct
  type 'p net = Tcp.channel -> (('p, serv) sess * all_empty, all_empty, unit) session
  open Session0
     
  let req conv cont tcp =
    recv () >>= fun v ->
    Tcp.send_line tcp (conv v);
    cont tcp
    
  let resp conv cont tcp =
    let line = Tcp.recv_line tcp in
    send (conv line) >>= fun () ->
    cont tcp
    
  let sel cont1 cont2 tcp =
    branch2
      (fun () -> cont1 tcp)
      (fun () -> cont2 tcp)
    
  let bra (conv1,cont1) cont2 tcp =
    let line = Tcp.recv_line tcp in
    match conv1 line with
    | Some(v) -> select_left () >> send v >>= fun () -> cont1 tcp
    | None -> cont2

  let cls tcp = Tcp.close tcp; close ()
                 
  let channel f opts =
    let ch = new_channel () in
    Thread.create (fun () ->
        accept_ ch (fun () ->
                  let tcp = Tcp.connect opts in
                  f tcp)) ();
    ch
end

type ehlo = EHLO of string
type mail = MAIL of string
type rcpt = RCPT of string
type data = DATA
type quit = QUIT

type r200 = R200 of string list
type r500 = R500 of string list
type R354 = R354 of string

module SMTP = struct
  open TcpSession
  let rec rcpt_part cont x =
    begin
      select (send rcpt @@ branch (r200, rcpt_part cont) (r500, send quit close))
             cont
    end @@ x
    
  let rec mail_loop x =
    begin
      send mail @@ recv r200 @@
        rcpt_part @@
          send data @@ recv r354 @@
            send string_list @@ recv r200 @@
              select (send quit close)
                     mail_loop
    end @@ x
    
  let smtp_protcol =
    recv r200 (send ehlo (recv r200, mail_loop ()))
end
            
let ch = TcpSession.channel SMTP.smtp_protocol opts
;;
  
  connect_ch (fun () ->
      send (EHLO("keigoimai.info")) >> recv () >>= fun (R200 str) ->
      send (MAIL("keigo.imai@gmail.com")) >> recv () >>= fun (R200 str) ->
      send (RCPT("keigoi@gifu-u.ac.jp")) >>
        branch (fun () ->
            recv () >>= fun (R200 str) ->
            send DATA >> recv () >>= fun R354 ->
            send (escape mailbody) >> recv () >>= fun (R200 str) ->
            send QUIT >>
            close ())
               (fun () ->
            recv () >>= fun (R500 str) ->
            List.iter print_endline str;
            send QUIT >>
            close ())) ()
                 
n
  *)                                                              
        

       
(* newtype EHLO = EHLO String; newtype MAIL = MAIL String; newtype RCPT = RCPT String *)
(* data    DATA = DATA;        data    QUIT = QUIT;         *)
(* -- Types for SMTP server replies (200 OK, 500 error and 354 start mail input) *)
(* newtype R2yz = R2yz [String]; newtype R5yz = R5yz [String]; newtype R354 = R354 String *)

(* module type SerialSession = sig *)
(*   type t *)
(*   val run : Tcp.channel -> ((t, serv) sess * all_empty, empty, unit) session *)
(* end *)

(* module type Printer = sig *)
(*   type v *)
(*   val print : v -> string *)
(* end *)

(* module type Parser = sig *)
(*   type v *)
(*   val parse : string -> v option *)
(* end *)
                          
(* module Send(V:Printer, P:SerialSession) : SerialSession *)
(*        with type t = [`msg of req * V.t * P.t] = *)
(* struct *)
(*   type t = [`msg of req * V.t * P.t] *)
(*   let run c = *)
(*     Session.recv () >>= fun x -> *)
(*     Tcp.send c (V.print x); *)
(*     P.run c *)
(* end;; *)
                          
(* module Recv(V:Parser, P:SerialSession) : SerialSession *)
(*        with type t = [`msg of resp * V.t * P.t] = *)
(* struct *)
(*   type t = [`msg of req * V.t * P.t] *)
(*   let run c = *)
(*     let str = Tcp.recv c in *)
(*     match V.parse str with *)
(*     | Some(x) -> *)
(*        Session.send (V.parse str) >> *)
(*          P.run c *)
(*     | None -> failwith "network parse error" *)
(* end;; *)

(* module Branch(V1:Parser, P1:SerialSession, V2:Parser, P2:SerialSession) : SerialSession *)
(*        with type t = [`branch of resp * [`left * [`msg of resp * V1.t * P1.t] *)
(*                                         | `right * [`msg of resp * V2.t * P2.t]]] = *)
(* struct *)
(*   type t =  [`branch of resp * [`left * [`msg of resp * V1.t * P1.t] *)
(*                                | `right * [`msg of resp * V2.t * P2.t]]] *)
(*   let run c = *)
(*     let str = Tcp.recv c in *)
(*     match parse str with *)
(*     | Some(x) -> *)
(*        Session.select_left () >> *)
(*          Session.send x >> *)
(*          P1.run c *)
(*     | None -> *)
       
    
            
    
(* end;;                                      *)
                               
                  
(* module Example4 = struct *)
(*   open SafeSession.Session0 *)
              
(*   let rec smtpclient ch ()  = *)
(*     _branch_start (function *)
(*                    | `EHLO(p,server) ->  *)
(* end *)
                
(*     _branch_start (function *)
(*        | `neg(p),r -> _branch (p,r) (neg_server ()) *)
(*        | `bin(p),r -> _branch (p,r) (binop_server ()) *)
(*        | `fin(p),r -> _branch (p,r) (close ()): [`neg of 'p1 | `bin of 'p2 | `fin of 'p3] * 'a -> 'b) *)
