open Session_ocaml
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

  let rec parse_reply kind f orig_str : 'a Session.parse_result  =
    let rec read acc str : 'a Session.parse_result =
      match Str.bounded_split_delim crlf str 2 with
      (* found CRLF *)
      | l::[ls] ->
         if l.[0] <> kind then
           `Fail(orig_str)
         else begin
             if l.[3] = '-' then
               read (msgbody l::acc) ls (* line continues *)
             else
               `Done(f (msgbody l::List.rev acc), ls)
         end
      (* no CRLF. starting over *)
      | _ -> `Partial(function Some(str) ->
                               parse_reply kind f (orig_str^str)
                             | None -> raise End_of_file (*FIXME*))
    in
    read [] orig_str

  let r200 str = parse_reply '2' (fun x -> R200(x)) str
  let r500 str = parse_reply '5' (fun x -> R500(x)) str
  let r354 str = parse_reply '3' (fun x -> R354(x)) str
  let ehlo (EHLO v) = "EHLO " ^ v ^ "\r\n"
  let mail (MAIL v) = "MAIL FROM:" ^ v ^ "\r\n"
  let rcpt (RCPT v) = "RCPT TO:" ^ v ^ "\r\n"
  let data DATA = "DATA\r\n"
  let quit QUIT = "QUIT\r\n"
  let string_list ls = String.concat "\r\n" ls ^ "\r\n.\r\n"
end


open SMTP_Commands
open Session
open UnixAdapter

type smtp = [`msg of resp * r200 * [`msg of req * ehlo * [`msg of resp * r200 *
    mail_loop]]]
and mail_loop = [`branch of req *
    [`left of [`msg of req * mail * [`msg of resp * r200 * rcpt_loop]]
    |`right of [`msg of req * quit * [`close]]]]
and rcpt_loop = [`branch of req * [`left of [`msg of req * rcpt *
      [`branch of resp * [`left of [`msg of resp * r200 * rcpt_loop]
        |`right of [`msg of resp * r500 * [`msg of req * quit * [`close]]]]]]
    |`right of body]]
and body = [`msg of req * data * [`msg of resp * r354 * [`msg of req * string list *
    [`msg of resp * r200 * mail_loop]]]]


let rec smtp_adapter : smtp net = fun ch -> (resp r200 @@ req ehlo @@ resp r200 @@ ml_p) ch
and ml_p ch = sel ~left:(req mail @@ resp r200 @@ rp_p) ~right:(req quit @@ cls) ch
and rp_p ch = sel ~left:(req rcpt @@ bra ~left:(r200, rp_p)
                                           ~right:(resp r500 @@ req quit @@ cls))
                 ~right:bd_p ch
and bd_p ch = (req data @@ resp r354 @@ req string_list @@ resp r200 @@ ml_p) ch

let escape : string -> string list = Str.split (Str.regexp "\n") (*FIXME*)

open Session
let smtp_client hostport from to_ mailbody =
  let ch : smtp channel = TcpSession.new_channel smtp_adapter hostport in
  connect_ ch begin fun () ->
    let%s R200 s = recv () in
    send (EHLO("me.example.com"))    >>  let%s R200 _ = recv () in
    select_left () >> (* enter into the main loop *)
    send (MAIL(from)) >>  let%s R200 _ = recv () in
    select_left () >> (* enter into recipient loop *)
    send (RCPT(to_))   >>
    branch2 (fun () -> let%s R200 _ = recv () in (* recipient Ok *)
                       select_right () >> (* proceed to sending the mail body *)
                       send DATA              >> let%s R354 _ = recv () in
                       send (escape mailbody) >> let%s R200 _ = recv () in
                       select_right () >> send QUIT >> close ())
            (fun () -> let%s R500 msg = recv () in (* a recipient is rejected *)
                       (print_endline "ERROR:"; List.iter print_endline msg; send QUIT) >> close ()) end ()


let () =
  if Array.length Sys.argv <> 5 then begin
      print_endline ("Usage: " ^ Sys.argv.(0) ^ " host:port from to body");
      exit 1
    end;
  smtp_client Sys.argv.(1) Sys.argv.(2) Sys.argv.(3) Sys.argv.(4)
