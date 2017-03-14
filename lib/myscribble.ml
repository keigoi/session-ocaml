open ScribbleSyntax

(* regular sub-grammar *)
type localtype =
  Close
| Send of role_name * (message_sig * localtype) list
| Recv of role_name * (message_sig * localtype) list
| Rec of string * localtype
| Cont of string

let local_of_scribblelocal =
  let flatten : role_name -> localtype list -> localtype = fun role typs ->
    let pick = function
    | Rec(_,_) -> failwith "found rec after choice" (* need loop unwind? *)
    | Cont(_) -> failwith "found cont after choice"
    | Close -> failwith "found end after choice" (* is this allowed? *)
    | Send(role,conts) -> `send, role, conts
    | Recv(role,conts) -> `recv, role, conts
    in
    let dir, role, conts =
      List.fold_left (fun (dir1,role1,conts1) (dir2,role2,conts2) ->
          if dir1<>None && dir1<>Some dir2 then
            failwith "incompatible send/receive in choice"
          else if dir2=`recv && role1<>role2 then
            failwith "incompatible destination role in choce"
          else
            (Some dir2,role2,conts1 @ conts2)
        ) (None,role,[]) (List.map pick typs)
    in
    match dir with
    | Some(`send) -> Send(role,conts)
    | Some(`recv) -> Recv(role,conts)
    | None -> failwith "choice is empty"
  in
  let rec conv_seq : as_local_protocol_body -> localtype -> localtype = fun t tail ->
    match t with
    | LASEnd -> tail
    | LASSend(_,msg,role) -> Send (role, [msg, tail])
    | LASRecv(_,msg,role) -> Recv (role, [msg, tail])
    | LASChoice(_,role,body) ->
       flatten role (List.map (fun l -> conv_seq l tail) body)
    | LASRec(_,ident,body) -> Rec(ident, conv_seq body tail)
    | LASSeq(t1, t2) -> conv_seq t1 (conv_seq t2 tail)
    | LASCont(_,ident) -> if tail=Close then Cont ident else failwith "local type must be tail-recursive"
    | LASPar(_,_) | LASInterrupt(_,_,_) -> failwith "not supported"
  in
  fun t -> conv_seq t Close

let variant_ : Parsetree.row_field list -> Parsetree.core_type = fun ts ->
  Ast_helper.Typ.variant ts Asttypes.Closed None

let mk_tag : string -> Parsetree.row_field = fun t ->
  Parsetree.Rtag(t,[],false,[])
                                                                   
let rec mk_tags_from_branch : message_sig * localtype -> Parsetree.row_field = fun ((op,payload),cont) ->
  let open Ast_helper.Typ in
  let payload =
    List.map (fun (_,typ) -> constr (Ast_convenience.lid typ) []) payload
  in
  let payload =
    match payload with
    | [] -> constr (Ast_convenience.lid "unit") []
    | [t] -> t
    | _ -> tuple payload
  in
  Parsetree.Rtag(op,[],false, [tuple [payload; typ_of_local cont]])
  
and typ_of_local : localtype -> Parsetree.core_type = fun typ ->
  let open Ast_helper.Typ in
  match typ with
  | Close -> [%type: [`close]]
  | Send(role,conts) ->
     [%type: [`send of [%t variant_ [mk_tag role] ] dir * [%t variant_ (List.map mk_tags_from_branch conts) ] ] ]
  | Recv(role,conts) ->
     [%type: [`recv of [%t variant_ [mk_tag role] ] dir * [%t variant_ (List.map mk_tags_from_branch conts) ] ] ]
  | Rec(ident, body) -> alias (typ_of_local body) ident
  | Cont ident -> var ident
