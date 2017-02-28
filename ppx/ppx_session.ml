(* open Printf *)
(* open Ast_mapper *)
(* open Ast_helper *)
open Asttypes
open Parsetree
(* open Longident *)
(* open Ast_convenience *)

let newname prefix i =
  Printf.sprintf "__ppx_session_%s_%d" prefix i

let monad_bind =
  [%expr [%e Ast_helper.Exp.ident (Ast_convenience.lid ("Session.>>="))]]

let error loc (s:string) = 
  Location.raise_errorf ~loc "%s" s
  
(* [?? = e0] and [?? = e1] and .. ==> [dum$0 = e0] and [dum$1 = e1] and .. 
   (to use with bindbody_of_let.) *)
let bindings_of_let bindings =
  List.mapi (fun i binding ->
      {binding with pvb_pat = Ast_convenience.pvar (newname "let" i)}
    ) bindings

(* [p0 = ??] and [p1 = ??] and .. and e ==> [bind dum$0 (fun p0 -> bind dum$1 (fun p1 -> (.. e)))] *)
let bindbody_of_let exploc bindings exp =
  let rec make i bindings =
    match bindings with
    | [] -> exp
    | binding :: t ->
      let name = (Ast_convenience.evar (newname "let" i)) [@metaloc binding.pvb_expr.pexp_loc] in
      let f = [%expr (fun [%p binding.pvb_pat] -> [%e make i t])] [@metaloc binding.pvb_loc] in
      let new_exp = [%expr [%e monad_bind] [%e name] [%e f]] [@metaloc exploc] in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  make 0 bindings
  
(* Converts match clauses to handle branching.
  | `lab1 -> e1
  | ..
  | `labN -> eN
  ==> 
  | `lab1(p),r -> _branch (p,r) e1
  | ..
  | `fin(p),r -> _branch (p,r) eN)
  : [`lab1 of 'p1 | .. | `labN of 'pN] * 'a -> 'b)
*)
let session_branch_clauses cases =
  let conv = function
    | {pc_lhs={ppat_desc=Ppat_variant(labl,pat);ppat_loc;ppat_attributes};pc_guard;pc_rhs=rhs_orig} ->
       if pat=None then
         let open Ast_convenience in
         let open Ast_helper in
         let open Ast_helper in
         let protocol_var = newname "match_p" 0 in
         let polarity_var = newname "match_q" 0 in
         let pat = [%pat? ( [%p Pat.variant labl (Some(pvar protocol_var)) ], [%p pvar polarity_var])] in
         let pair = [%expr [%e evar protocol_var],[%e evar polarity_var]] in
         let expr = [%expr Session.Session0._branch [%e pair] [%e rhs_orig]] in
         {pc_lhs={ppat_desc=pat.ppat_desc;ppat_loc;ppat_attributes};pc_guard;pc_rhs=expr}, labl
       else
         error ppat_loc "Invalid variant pattern"
    | {pc_lhs={ppat_loc=loc}} -> error loc "Invalid pattern"
  in
  List.split (List.map conv cases)


let expression_mapper id mapper exp attrs =
  let open Ast_mapper in
  let pexp_attributes = attrs @ exp.pexp_attributes in
  let pexp_loc=exp.pexp_loc in
  match id, exp.pexp_desc with
    
  (* let%s p = e1 in e2 ==> let dum$0 = e1 in Session.(>>=) dum$0 e2 *)
  | "s", Pexp_let (Nonrecursive, vbl, expression) ->
      let new_exp =
        Ast_helper.Exp.let_
          Nonrecursive
          (bindings_of_let vbl)
          (bindbody_of_let exp.pexp_loc vbl expression)
      in
      Some (mapper.expr mapper { new_exp with pexp_attributes })
  | "s", _ -> error pexp_loc "Invalid content for extension %s"
      
  (* [%select0 `labl] ==> _select (fun x -> `labl(x)) *)
  | "select0", Pexp_variant (labl, None) ->
     let new_exp =
       [%expr Session.Session0._select
              (fun [%p Ast_convenience.pvar "x"] ->
                [%e Ast_helper.Exp.variant labl (Some(Ast_convenience.evar "x"))]) ]
     in
     Some (mapper.expr mapper {new_exp with pexp_attributes})
  | "select0", _ -> error pexp_loc "Invalid content for extension %select0"
     
  (*
  match%branch0 () with
  | `lab1 -> e1
  | ..
  | `labN -> eN
  ==> 
  _branch_start ((function
     | `lab1(p),r -> _branch (p,r) e1
     | ..
     | `fin(p),r -> _branch (p,r) eN)
     : [`lab1 of 'p1 | .. | `labN of 'pN] * 'a -> 'b)
  *)
  | "branch0", Pexp_match ({pexp_desc=Pexp_construct({txt=Longident.Lident("()")},None)}, cases) ->
     let open Ast_helper.Typ in
     let cases, labls = session_branch_clauses cases in
     let rows = List.mapi (fun i labl -> Rtag(labl,[],false,[var ("p"^string_of_int i)])) labls in
     let new_typ =
       [%type: [%t (variant rows Closed None)] * 'a -> 'b ]
     in
     let new_exp =
       [%expr Session.Session0._branch_start ([%e Ast_helper.Exp.function_ cases] : [%t new_typ ])]
    in
    Some (mapper.expr mapper {new_exp with pexp_attributes})
  | "branch0", _ -> error pexp_loc "Invalid content for extension %branch0"
     
  | _ -> None

let mapper_fun _ =
  let open Ast_mapper in
  let expr mapper outer =
  match outer.pexp_desc with
  | Pexp_extension ({ txt = id; loc }, PStr [{ pstr_desc = Pstr_eval (inner, attrs) }]) ->
     begin match expression_mapper id mapper inner attrs with
     | Some exp -> exp
     | None -> default_mapper.expr mapper outer
     end
  | _ -> default_mapper.expr mapper outer
  in  
  {default_mapper with expr}
