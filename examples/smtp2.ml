(* ocamlfind ocamlc -o smtp -linkpkg -short-paths -thread -package linocaml.ppx,linocaml.ppx_lens,session_ocaml smtp.ml *)
open Dsession
open Dsession.Tcp

let bufsize = 4096

(* EHLO example.com *)                (* MAIL FROM: alice@example.com *)
type ehlo = EHLO of string            type mail = MAIL of string
(* RCPT TO: bob@example.com *)        (* DATA *)
type rcpt = RCPT of string            type data_ = DATA
(* QUIT *)                            (* Success e.g. 250 Ok *)
type quit = QUIT                      type r200 = R200 of string list
(* Error e.g. 554 Relay denied *)     (* 354 Start mail input *)
type r500 = R500 of string list       type r354 = R354 of string list
(* mail body *)
type mailbody = MailBody of string

(* Instance declaration for SMTP reply deserialisers *)
module Receivers = struct


  let rec parse_reply table tcp =
    let read_chunk () =
      let rec read_chunk_aux lines =
        let line = input_line tcp.in_ in
        (* remove trailing \r *)
        let line =
          let len = String.length line in
          if len > 0 && line.[len-1] = '\r'
          then String.sub line 0 (len-1)
          else line
        in
        if line.[3] = '-' then
          (* chunk continues. get the next line *)
          read_chunk_aux (line::lines)
        else
          (* chunk terminated *)
          List.rev (line::lines)
      in
      read_chunk_aux []
    in
    let lines = read_chunk () in
    match lines with
    | [] -> assert false
    | first::_ ->
       if String.length first = 0 then
         failwith ("Parse error :" ^ String.concat "\r\n" lines)
       else
         (* get message constructor from the table*)
         let f =
           try
             List.assoc first.[0] table
           with
           | Not_found ->
              failwith ("Parse error : expected "
                        ^ String.concat " or " (List.map Char.escaped (fst (List.split table)))
                        ^ " but got:\r\n" ^ String.concat "\r\n" lines)
         in
         let msgbody s = String.sub s 4 (String.length s-4)
         in
         f tcp (List.map msgbody lines)

  let r200 = ('2', (fun tcp msg -> `_200(W msg, _mksess tcp)))
  let r354 = ('3', (fun tcp msg -> `_354(W msg, _mksess tcp)))
  let r500 = ('5', (fun tcp msg -> `_500(W msg, _mksess tcp)))

  let _200
      : stream -> [`_200 of _ * _]
    = fun c -> parse_reply [r200] c
  let _200_or_500
      : stream -> [`_200 of _ * _ | `_500 of _ * _] 
    = fun c -> parse_reply [r200; r500] c
  let _354
      : stream -> [`_354 of _ * _]
    = fun c -> parse_reply [r354] c

end

(* Instance declaration for SMTP command serialisers *)
module Senders = struct
  let write {out} str =
    output_string out str;
    flush out

  let _ehlo c (`EHLO (v,_) : [`EHLO of _]) = write c @@ "EHLO " ^ v ^ "\r\n"
  let _mailbody c (MailBody s) = write c @@ s ^ "\r\n.\r\n"

  let _mail c : [`MAIL of _] -> unit = function
    | `MAIL(v,_) -> write c @@ "MAIL FROM:" ^ v ^ "\r\n"

  let _quit c : [`QUIT of _] -> unit = function
    | `QUIT(_) -> write c "QUIT\r\n"

  let _rcpt_or_data c = (function
    | `RCPT(v,_) -> write c @@ "RCPT TO:" ^ v ^ "\r\n"
    | `DATA(_) -> write c "DATA\r\n"
                  : [`RCPT of _ | `DATA of _] -> unit)
                      
  let _mail_or_quit c = (function
    | `MAIL(_) as m -> _mail c m
    | `QUIT(_) as m -> _quit c m
                : [`MAIL of _ | `QUIT of _] -> unit)

end  


type 'p cont = ('p,cli,stream) dsess
type 'p contR = ('p,serv,stream) dsess

type smtp =
  [`branch of resp * [`_200 of string list data *
  [`branch of req * [`EHLO of string *
  [`branch of resp * [`_200 of string list data *
  mailloop cont]] contR]] cont]]
and mailloop =
  [`branch of req *
    [`MAIL of string *
      [`branch of resp * [`_200 of string list data * rcptloop cont]] contR
    |`QUIT of
      [`close] contR]]
and rcptloop =
  [`branch of req * 
    [`RCPT of string *
      [`branch of resp * 
        [`_200 of string list data * 
          rcptloop cont
        |`_500 of string list data *
          [`branch of req * [`QUIT of 
          [`close] contR]] cont]] contR
    |`DATA of 
      [`branch of resp * [`_354 of string list data *
      [`msg of req * mailbody * 
      [`branch of resp * [`_200 of string list data *
      mailloop cont]]] cont]] contR]]

open Tcp

let s = _0

let sendmail host port from to_ mailbody () : ((smtp,cli,stream) dsess * empty_three, empty_four, unit lin) lmonad =
  let%lin `_200(msg,#s) = branch s in
  List.iter print_endline msg;
  let%lin #s = select (fun x -> `EHLO("me.example.com",x)) s in
  let%lin `_200(_,#s) =  branch s in
  let%lin #s = select (fun x -> `MAIL(from,x)) s in
  let%lin `_200(_,#s) = branch s in
  let%lin #s = select (fun x -> `RCPT(to_,x)) s in
  begin match%lin branch s with
  | `_200(_,#s) ->
     let%lin #s = select (fun x -> `DATA(x)) s in
     let%lin `_354(_, #s) = branch s in
     let%lin #s = send (MailBody mailbody) s in
     let%lin `_200(msg, #s) = branch s in
     print_string "Email sent: ";
     List.iter print_endline msg;
     let%lin #s = select (fun x -> `QUIT(x)) s in
     return ()
  | `_500 (msg,#s) ->
     print_endline "Email sending failed. Detail:";
     List.iter print_endline msg;
     let%lin #s = select (fun x -> `QUIT(x)) s in
     return ()
  end >>
  close s
  
let smtp_client host port from to_ mailbody () =
  let smtp : (smtp,stream) connector = connector ~host ~port in
  let%lin #s = connect smtp in
  sendmail host port from to_ mailbody ()

let () =
  if Array.length Sys.argv <> 6 then begin
      print_endline ("Usage: " ^ Sys.argv.(0) ^ " host port from to body");
      exit 1
    end;
  run (smtp_client Sys.argv.(1) (int_of_string Sys.argv.(2)) Sys.argv.(3) Sys.argv.(4) Sys.argv.(5))
