open ScribbleSyntax

(* regular sub-grammar *)
type localtype =
  Close
| Send of message_sig * role_name * localtype
| Recv of message_sig * role_name * localtype
| Choice of role_name * localtype list
| Rec of string * localtype
| Cont of string

let local_of_scribblelocal =
  let rec conv_seq : as_local_protocol_body -> localtype -> localtype = fun t tail ->
    match t with
    | LASEnd -> tail
    | LASSend(_,msg,role) -> Send(msg,role,tail)
    | LASRecv(_,msg,role) -> Recv(msg,role,tail)
    | LASChoice(_,role,body) -> Choice(role, List.map (fun t1 -> conv_seq t1 tail) body)
    | LASRec(_,ident,body) -> Rec(ident, conv_seq body tail)
    | LASSeq(t1, t2) -> conv_seq t1 (conv_seq t2 Close)
    | LASCont(_,_) -> failwith "local type must be tail-recursive"
    | LASPar(_,_) | LASInterrupt(_,_,_) -> failwith "not supported"
  in
  fun t -> conv_seq t Close

let typ_of_local = function
  | Close -> [%type: [`close]]
  | _ -> failwith "TODO"
  

(*
  | LASEnd
  | LASSend of (info * message_sig * role_name)
  | LASRecv of (info * message_sig * role_name)
  | LASSeq of (as_local_protocol_body * as_local_protocol_body)
  | LASChoice of (info * role_name * (as_local_protocol_body list))
  | LASPar of (info * (as_local_protocol_body list))
  | LASRec of (info * string * as_local_protocol_body)
  | LASCont of info * string
  | LASInterrupt of (info * as_local_protocol_body * ((role_name * message_sig) list))

type as_local =
    string * role_name * parameters * roles * as_local_protocol_body
    *)  
