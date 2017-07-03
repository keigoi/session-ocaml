open Ast_helper
open Asttypes
open Parsetree
(* open Longident *)
open Ast_convenience

let newname prefix i =
  Printf.sprintf "__ppx_session_%s_%d" prefix i

let freshname =
  let r = ref 0 in
  fun () ->
    let i = !r in
    r := i + 1;
    Printf.sprintf "ppx_session_var_%d" i
  
let root_module = ref "Session.Syntax"

let longident str = Exp.ident (lid str)

let monad_bind () =
  longident (!root_module ^ ".>>=")

let error loc (s:string) = 
  Location.raise_errorf ~loc "%s" s
  
(* [?? = e0] and [?? = e1] and .. ==> [dum$0 = e0] and [dum$1 = e1] and .. 
   (to use with bindbody_of_let.) *)
let bindings_of_let bindings =
  List.mapi (fun i binding ->
      {binding with pvb_pat = pvar (newname "let" i)}
    ) bindings

(* [p0 = ??] and [p1 = ??] and .. and e ==> [bind dum$0 (fun p0 -> bind dum$1 (fun p1 -> .. -> e))] *)
let bindbody_of_let exploc bindings exp =
  let rec make i bindings =
    match bindings with
    | [] -> exp
    | binding :: t ->
      let name = (evar (newname "let" i)) [@metaloc binding.pvb_expr.pexp_loc] in
      let f = [%expr (fun [%p binding.pvb_pat] -> [%e make (i+1) t])] [@metaloc binding.pvb_loc] in
      let new_exp = [%expr [%e monad_bind ()] [%e name] [%e f]] [@metaloc exploc] in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  make 0 bindings

(* [{lab1} = e1] and [{lab2} = e2 and .. and e ==> e1 ~bindto:lab1 >>= (fun () -> e2 ~bindto:lab2 ]  *)
let slot_bind bindings expr =
  let f binding expr =
    match binding with
    | {pvb_pat = {ppat_desc = Ppat_record ([({txt},_)],Closed)}; pvb_expr = rhs}
    | {pvb_pat = {ppat_desc = Ppat_type {txt}}; pvb_expr = rhs} ->
      let lensname = String.concat "." (Longident.flatten txt) in
      let f = Exp.fun_ Label.nolabel None (punit ()) expr in
      [%expr [%e monad_bind ()] ([%e rhs] ~bindto:[%e evar lensname]) [%e f]]
    | _ -> raise Not_found
  in List.fold_right f bindings expr
  
(* Generates session selection. [%select `labl] ==> _select0 (fun x -> `labl(x))  *)
let session_select which labl =
  let selectfunc =
    match which with
    | `Session0 -> longident (!root_module ^ ".Session0._select")
    | `SessionN e -> [%expr [%e longident (!root_module ^ ".SessionN._select")] [%e e]]
  in
  let new_exp =
    [%expr [%e selectfunc ]
           (fun [%p pvar "x"] ->
             [%e Exp.variant labl (Some(evar "x"))]) ]
  in new_exp
  
(* Converts match clauses to handle branching.
  | `lab1 -> e1
  | ..
  | `labN -> eN
  ==> 
  | `lab1(p),r -> _branch e0? (p,r) e1
  | ..
  | `fin(p),r -> _branch e0? (p,r) eN)
  : [`lab1 of 'p1 | .. | `labN of 'pN] * 'a -> 'b)
*)
let session_branch_clauses which cases =
  let branch_exp =
    match which with
    | `Session0 -> longident (!root_module ^ ".Session0._branch")
    | `SessionN e -> [%expr [%e longident (!root_module ^ ".SessionN._branch")] [%e e]]
  in
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
         let expr = [%expr [%e branch_exp] [%e pair] [%e rhs_orig]] in
         {pc_lhs={ppat_desc=pat.ppat_desc;ppat_loc;ppat_attributes};pc_guard;pc_rhs=expr}, labl
       else
         error ppat_loc "Invalid variant pattern"
    | {pc_lhs={ppat_loc=loc}} -> error loc "Invalid pattern"
  in
  List.split (List.map conv cases)

let branch_func_name = function
  | `Session0 -> longident (!root_module ^ ".Session0._branch_start")
  | `SessionN -> longident (!root_module ^ ".SessionN._branch_start")

let make_branch_func_types labls =
  let open Typ in
  let rows =
    List.mapi (fun i labl -> Rtag(labl,[],false,[var ("p"^string_of_int i)])) labls
  in
  [%type: [%t (variant rows Closed None)] * [%t var (freshname ())] -> [%t var (freshname ())] ]

let expression_mapper id mapper exp attrs =
  let pexp_attributes = attrs @ exp.pexp_attributes in
  let pexp_loc=exp.pexp_loc in
  match id, exp.pexp_desc with

  (* monadic bind *)
  (* let%s p = e1 in e2 ==> let dum$0 = e1 in Session.(>>=) dum$0 e2 *)
  | ("s"|"w"), Pexp_let (Nonrecursive, vbl, expression) ->
      let new_exp =
        Exp.let_
          Nonrecursive
          (bindings_of_let vbl)
          (bindbody_of_let exp.pexp_loc vbl expression)
      in
      Some (mapper.Ast_mapper.expr mapper { new_exp with pexp_attributes })
  | ("s"|"w"), _ -> error pexp_loc "Invalid content for extension %s|%w"

  (* slot bind *)
  (* let%lin {lab} = e1 in e2 ==> Session.(>>=) (e1 ~bindto:lab) (fun () -> e2) *)
  | "lin", Pexp_let (Nonrecursive, vbl, expression) ->
      let new_exp = slot_bind vbl expression in
      Some (mapper.Ast_mapper.expr mapper { new_exp with pexp_attributes })
  | "lin", _ -> error pexp_loc "Invalid content for extension %lin"

  (* session selection *)
  (* [%select0 `labl] ==> _select (fun x -> `labl(x)) *)
  | "select0", Pexp_variant (labl, None) ->
     let new_exp = session_select `Session0 labl in
     Some (mapper.Ast_mapper.expr mapper {new_exp with pexp_attributes})
  | "select0", _ -> error pexp_loc "Invalid content for extension %select0"

  (* [%select _n `labl] ==> _select _n (fun x -> `labl(x)) *)
  | "select", Pexp_apply(e1, [(_,{pexp_desc=Pexp_variant (labl, None)})]) ->
     let new_exp = session_select (`SessionN e1) labl in
     Some (mapper.Ast_mapper.expr mapper {new_exp with pexp_attributes})
  | "select", Pexp_variant(labl1, Some {pexp_desc=Pexp_variant (labl2, None)}) ->
     let new_exp = session_select (`SessionN (Exp.variant labl1 None)) labl2 in
     Some (mapper.Ast_mapper.expr mapper {new_exp with pexp_attributes})
  | "select", _ -> error pexp_loc "Invalid content for extension %select"
     
  (* session branching
  match%branch0 () with | `lab1 -> e1 | .. | `labN -> eN
  ==>
  _branch_start ((function
     | `lab1(p),r -> _branch (p,r) e1 | ..
     | `labN(p),r -> _branch (p,r) eN)
     : [`lab1 of 'p1 | .. | `labN of 'pN] * 'a -> 'b)
  *)
  | "branch0", Pexp_match ({pexp_desc=Pexp_construct({txt=Longident.Lident("()")},None)}, cases) ->
     let cases, labls = session_branch_clauses `Session0 cases in
     let new_typ = make_branch_func_types labls in
     let new_exp =
       [%expr [%e branch_func_name `Session0] ([%e Exp.function_ cases] : [%t new_typ ])]
    in
    Some (mapper.Ast_mapper.expr mapper {new_exp with pexp_attributes})
  | "branch0", _ -> error pexp_loc "Invalid content for extension %branch0"

  (*
  match%branch e0 with | `lab1 -> e1 | .. | `labN -> eN
  ==>
  _branch_start e0 ((function
     | `lab1(p),r -> _branch e0 (p,r) e1 | ..
     | `labN(p),r -> _branch e0 (p,r) eN)
     : [`lab1 of 'p1 | .. | `labN of 'pN] * 'a -> 'b)
  *)
  | "branch", Pexp_match (e0, cases) ->
     let open Typ in
     let cases, labls = session_branch_clauses (`SessionN e0) cases in
     let new_typ = make_branch_func_types labls in
     let new_exp =
       [%expr [%e branch_func_name `SessionN] [%e e0]
              ([%e Exp.function_ cases] : [%t new_typ ])]
    in
    Some (mapper.Ast_mapper.expr mapper {new_exp with pexp_attributes})
  | "branch", _ -> error pexp_loc "Invalid content for extension %branch"

  | _ -> None

let rebind_module modexpr =
  match modexpr.pmod_desc with
  | Pmod_ident {txt = id} -> root_module := String.concat "." (Longident.flatten id)
  | _ -> error modexpr.pmod_loc "Use (module M) here."
  
       
let mapper_fun _ =
  let open Ast_mapper in
  let expr mapper outer =
  match outer.pexp_desc with
  | Pexp_extension ({ txt = id }, PStr [{ pstr_desc = Pstr_eval (inner, attrs) }]) ->
     begin match expression_mapper id mapper inner attrs with
     | Some exp -> exp
     | None -> default_mapper.expr mapper outer
     end
  | _ -> default_mapper.expr mapper outer
  and structure_item mapper outer =
    match outer.pstr_desc with
    | Pstr_extension (({ txt = "s_syntax_rebind" }, PStr [{ pstr_desc = Pstr_eval ({pexp_desc=Pexp_pack modexpr}, _) }]),_) ->
       rebind_module modexpr;
       {outer with pstr_desc = Pstr_eval ([%expr ()],[])} (* replace with () *)
    | _ -> default_mapper.structure_item mapper outer
  in
  {default_mapper with expr; structure_item}
