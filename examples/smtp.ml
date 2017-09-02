(* ocamlfind ocamlc -o smtp -linkpkg -short-paths -thread -package linocaml.ppx,linocaml.ppx_lens,session_ocaml smtp.ml *)
open Linocaml.Direct
open Session_ocaml.Direct.Net

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

  let crlf = (Str.regexp "\r\n")
  let msgbody s = String.sub s 4 (String.length s-4)

  let rec parse_reply assoc tcp =
    let buf = Bytes.create bufsize in
    let read_buf () =
      let len_read = input tcp.in_ buf 0 bufsize in
      Bytes.sub_string buf 0 len_read
    in
    let get_line = 
      let rec get_line_aux str leftover =
        match Str.bounded_split_delim crlf leftover 2 with (* splits "L1\r\nL2\r\nL3" into ["L1"; "L2\r\nL3"] *)
        | [line; rest] ->  
           line, rest
        | [] -> (* no CRLF. read further *)
           let read = read_buf () in
           if read<>"" then
             get_line_aux (str^leftover) read (* try to find CRLF again *)
           else
             str^leftover, "" (* EOF *)
        | _ -> assert false
      in
      get_line_aux ""
    in
    let read_chunk = 
      let rec read_chunk_aux lines left =
        let line, rest = get_line left in
        if line.[3] = '-' then
          (* chunk continues. get the next line *)
          read_chunk_aux (line::lines) rest
        else
          (* chunk terminated *)
          List.rev (line::lines), rest
      in
      read_chunk_aux []
    in
    let lines, left = read_chunk tcp.in_leftover in
    tcp.in_leftover <- left;
    match lines with
    | [] -> assert false
    | first::_ ->
       if String.length first = 0 then
         failwith ("Parse error :" ^ String.concat "\r\n" lines)
       else
         let f =
           try
           List.assoc first.[0] assoc
         with
         | Not_found ->
            failwith ("Parse error : expected "
                      ^ String.concat " or " (List.map Char.escaped (fst (List.split assoc)))
                      ^ " but got:\r\n" ^ String.concat "\r\n" lines)
         in
         f tcp (List.map msgbody lines)

  let r200 = ('2', (fun c x -> `_200(Linocaml.Base.Data_Internal__ x,_mksess c)))
  let r354 = ('3', (fun c x -> `_354(Linocaml.Base.Data_Internal__ x,_mksess c)))
  let r500 = ('5', (fun c x -> `_500(Linocaml.Base.Data_Internal__ x,_mksess c)))
  let _200 c : [`_200 of _ * _] = parse_reply [r200] c
  let _200_or_500 c : [`_200 of _ * _ | `_500 of _ * _] = parse_reply [r200; r500] c
  let _354 c : [`_354 of _ * _] = parse_reply [r354] c

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


type 'p cont = ('p,cli,tcp) sess

type smtp =
  [`branch of resp * [`_200 of string list data *
  [`branch of req * [`EHLO of string *
  [`branch of resp * [`_200 of string list data *
  mailloop cont]] cont]] cont]]
and mailloop =
  [`branch of req *
    [`MAIL of string *
      [`branch of resp * [`_200 of string list data * rcptloop cont]] cont
    |`QUIT of
      [`close] cont]]
and rcptloop =
  [`branch of req * 
    [`RCPT of string *
      [`branch of resp * 
        [`_200 of string list data * 
          rcptloop cont
        |`_500 of string list data *
          [`branch of req * [`QUIT of 
          [`close] cont]] cont]] cont
    |`DATA of 
      [`branch of resp * [`_354 of string list data *
      [`msg of req * mailbody * 
      [`branch of resp * [`_200 of string list data *
      mailloop cont]]] cont]] cont]]

(* declare slot s *)
type 'a t = <s:'a>[@@runner][@@deriving lens]

open Tcp

(* let tcp : (('p,cli,tcp) sess, ('p,cli,tcp) sess, 'pre, 'pre) slot -> ('pre, 'pre, unit lin) monad = fun _ -> return () *)
let tcp : (('p,cli,tcp) sess, empty, 'pre, 'post) slot -> ('pre, 'post, ('p,cli,tcp) sess) monad = fun s -> get s

let smtp_client host port from to_ mailbody () =
  let smtp : (smtp,tcp) connector = connector ~host ~port in
  let%lin #s = connect smtp in
  let%lin `_200(msg,#s) = branch s in
  List.iter print_endline msg;
  select s (fun x -> `EHLO("me.example.com",x)) >>
  let%lin `_200(_,#s) =  branch s in
  select s (fun x -> `MAIL(from,x)) >>
  let%lin `_200(_,#s) = branch s in
  select s (fun x -> `RCPT(to_,x)) >>
  branch s >>=
    (function%lin
           | `_200(_,#s) ->
              select s (fun x -> `DATA(x)) >>
              let%lin `_354(_, #s) = branch s in
              send s (MailBody mailbody) >>
              let%lin `_200(msg, #s) = branch s in
              print_string "Email sent: ";
              List.iter print_endline msg;
              select s (fun x -> `QUIT(x))
           | `_500 (msg,#s) ->
              print_endline "Email sending failed. Detail:";
              List.iter print_endline msg;
              select s (fun x -> `QUIT(x))
              : ([`_200 of _ | `_500 of _] lin,_,_,_) bindfun) >>
  close s

let () =
  if Array.length Sys.argv <> 6 then begin
      print_endline ("Usage: " ^ Sys.argv.(0) ^ " host port from to body");
      exit 1
    end;
  run_t (smtp_client Sys.argv.(1) (int_of_string Sys.argv.(2)) Sys.argv.(3) Sys.argv.(4) Sys.argv.(5)) ()
