open Linocaml.Direct
open Session_ocaml.Direct.Net

let bufsize = 4096

(* EHLO example.com *)                (* MAIL FROM: alice@example.com *)
type ehlo = EHLO of string            type mail = MAIL of string
(* RCPT TO: bob@example.com *)        (* DATA *)
type rcpt = RCPT of string            type data = DATA
(* QUIT *)                            (* Success e.g. 250 Ok *)
type quit = QUIT                      type r200 = R200 of string list
(* Error e.g. 554 Relay denied *)     (* 354 Start mail input *)
type r500 = R500 of string list       type r354 = R354 of string list

module SMTP_Commands = struct

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
  let r354 = ('3', (fun c x -> `_354(x,_mksess c)))
  let r500 = ('5', (fun c x -> `_500(x,_mksess c)))
  let _200 c : [<`_200 of string list Linocaml.Base.data * ('p,'q,tcp) sess] = parse_reply [r200] c
  let _200_or_500 c = parse_reply [r200; r500] c
  let _354 c = parse_reply [r354] c

  let write str {out} = output out str
           
  let _ehlo (`EHLO v)  = write @@ "EHLO " ^ v ^ "\r\n"
  let _mail (`MAIL v)  = write @@ "MAIL FROM:" ^ v ^ "\r\n"
  let _rcpt (`RCPT v)  = write @@ "RCPT TO:" ^ v ^ "\r\n"
  let _data `DATA      = write "DATA\r\n"
  let _quit `QUIT      = write "QUIT\r\n"
  let _string_list ls = write @@ String.concat "\r\n" ls ^ "\r\n.\r\n"
end


open SMTP_Commands

   
(* type smtp = [`msg of resp * r200 * [`msg of req * ehlo * [`msg of resp * r200 * *)
(*     mail_loop]]] *)
(* and mail_loop = [`branch of req * *)
(*     [`left of [`msg of req * mail * [`msg of resp * r200 * rcpt_loop]] *)
(*     |`right of [`msg of req * quit * [`close]]]] *)
(* and rcpt_loop = [`branch of req * [`left of [`msg of req * rcpt * *)
(*       [`branch of resp * [`left of [`msg of resp * r200 * rcpt_loop] *)
(*         |`right of [`msg of resp * r500 * [`msg of req * quit * [`close]]]]]] *)
(*     |`right of body]] *)
(* and body = [`msg of req * data * [`msg of resp * r354 * [`msg of req * string list * *)
(*     [`msg of resp * r200 * mail_loop]]]] *)


let escape : string -> string list = Str.split (Str.regexp "\n") (*FIXME*)

type 'a t = <s:'a>
[@@runner][@@deriving lens]

open Tcp

let smtp_client host port from to_ mailbody =
  let smtp : ('p,tcp) connector = connector ~host ~port in
  let%lin #s = connect smtp in
  let%lin (`_200(_,#s)) = branch s in
  close s
                                   
(* let smtp_client host port from to_ mailbody = *)
(*   let smtp : ('p,tcp) connector = Tcp.connector ~host ~port in *)
(*   let%lin #s = connect smtp in *)
(*     let%lin (`_200(#s)) = branch s in *)
(*     send s (EHLO("me.example.com")) >>  let%s R200 _ = recv () in *)
(*     select_left () >> (\* enter into the main loop *\) *)
(*     send (MAIL(from)) >>  let%s R200 _ = recv () in *)
(*     select_left () >> (\* enter into recipient loop *\) *)
(*     send (RCPT(to_))   >> *)
(*     branch2 (fun () -> let%s R200 _ = recv () in (\* recipient Ok *\) *)
(*                        select_right () >> (\* proceed to sending the mail body *\) *)
(*                        send DATA              >> let%s R354 _ = recv () in *)
(*                        send (escape mailbody) >> let%s R200 _ = recv () in *)
(*                        select_right () >> send QUIT >> close ()) *)
(*             (fun () -> let%s R500 msg = recv () in (\* a recipient is rejected *\) *)
(*                        (print_endline "ERROR:"; List.iter print_endline msg; send QUIT) >> close ()) end () *)


(* let () = *)
(*   if Array.length Sys.argv <> 5 then begin *)
(*       print_endline ("Usage: " ^ Sys.argv.(0) ^ " host:port from to body"); *)
(*       exit 1 *)
(*     end; *)
(*   smtp_client Sys.argv.(1) Sys.argv.(2) Sys.argv.(3) Sys.argv.(4) *)
