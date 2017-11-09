(* TODO: replace "failwith" with proper error-handling *)

open Asttypes
open Parsetree
open Longident
open Ast_helper
open Ast_convenience

let newname =
  let r = ref 0 in
  fun prefix ->
    let i = !r in
    r := i + 1;
  Printf.sprintf "__ppx_linocaml_%s_%d" prefix i
  
let root_module = ref "Syntax"

let longident ?loc str = evar ?loc str

let monad_bind () =
  longident (!root_module ^ ".bind")

let emptyslot () =
  longident (!root_module ^ ".empty")

let mkbindfun () =
  longident (!root_module ^ ".Internal.__mkbindfun")

let monad_bind_raw () =
  longident (!root_module ^ ".Internal.__bind_raw")

let monad_return_raw () =
  longident (!root_module ^ ".Internal.__return_raw")
  
let setfunc_raw () =
  longident (!root_module ^ ".Internal.__putval_raw")
  
let getfunc () =
  longident (!root_module ^ ".Internal.__takeval_raw")
  
let error loc (s:string) =
  Location.raise_errorf ~loc "%s" s

let rec traverse f(*var wrapper*) g(*#tconst wrapper*) ({ppat_desc} as patouter) =
  match ppat_desc with
  | Ppat_any -> f patouter
        (* _ *)
  | Ppat_var _ -> f patouter
        (* x *)
  | Ppat_alias (pat,tvarloc) ->
     error tvarloc.loc "as-pattern is forbidden at %lin match" (* TODO relax this *)
     (* {patouter with ppat_desc=Ppat_alias(traverse f g pat,tvarloc)} *)
        (* P as 'a *)
  | Ppat_constant _ -> patouter
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval (_,_) -> patouter
        (* 'a'..'z'

           Other forms of interval are recognized by the parser
           but rejected by the type-checker. *)
  | Ppat_tuple pats -> {patouter with ppat_desc=Ppat_tuple(List.map (traverse f g) pats)}
        (* (P1, ..., Pn)

           Invariant: n >= 2
        *)
  | Ppat_construct (lidloc,Some(pat)) -> {patouter with ppat_desc=Ppat_construct(lidloc,Some(traverse f g pat))}
  | Ppat_construct (_,None) -> patouter
        (* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
         *)
  | Ppat_variant (lab,Some(pat)) -> {patouter with ppat_desc=Ppat_variant(lab,Some(traverse f g pat))}
  | Ppat_variant (lab,None) -> patouter
        (* `A             (None)
           `A P           (Some P)
         *)
  | Ppat_record (recpats, Closed) ->
     {patouter with
       ppat_desc=Ppat_record(List.map (fun (field,pat) -> (field,traverse f g pat)) recpats, Closed)
     }
        (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)

           Invariant: n > 0
         *)
  | Ppat_array pats -> {patouter with ppat_desc=Ppat_array (List.map (traverse f g) pats)}
        (* [| P1; ...; Pn |] *)
  | Ppat_constraint (pat,typ)  -> {patouter with ppat_desc=Ppat_constraint(traverse f g pat,typ)}
        (* (P : T) *)
  | Ppat_type lidloc -> g lidloc
        (* #tconst *)
  | Ppat_lazy pat -> {patouter with ppat_desc=Ppat_lazy(traverse f g pat)}
                   
  | Ppat_record (_, Open)
  | Ppat_or (_,_) | Ppat_unpack _
  | Ppat_exception _ | Ppat_extension _ | Ppat_open _ ->
       error patouter.ppat_loc "%lin cannot handle this pattern"

let rec is_linpat {ppat_desc;ppat_loc} = 
  match ppat_desc with
  | Ppat_type _ -> true
  | Ppat_alias (pat,_) -> is_linpat pat
  | Ppat_constraint (pat,_)  -> is_linpat pat
  | Ppat_any | Ppat_var _ 
    | Ppat_constant _ | Ppat_interval (_,_)
    | Ppat_tuple _ | Ppat_construct (_,_)
    | Ppat_variant (_,_) | Ppat_record (_, _)
    | Ppat_array _ | Ppat_lazy _ -> false
  | Ppat_or (_,_) | Ppat_unpack _
    | Ppat_exception _ | Ppat_extension _ | Ppat_open _ ->
     error ppat_loc "%lin cannot handle this pattern"
  
let lin_pattern oldpat =
  let wrap ({ppat_loc} as oldpat) =
    let lin_vars = ref []
    in
    let replace_linpat ({loc} as linvar) =
      let newvar = newname "match" in
      lin_vars := (linvar,newvar) :: !lin_vars;
      pconstr ~loc "L" [pvar ~loc newvar]
      
    and wrap_datapat ({ppat_loc} as pat) =
      pconstr ~loc:ppat_loc "W" [pat]
    in
    let newpat = traverse wrap_datapat replace_linpat oldpat in
    let newpat =
      if is_linpat oldpat then
        newpat (* not to duplicate Lin pattern *)
      else
        pconstr ~loc:ppat_loc "L" [newpat]
    in
    newpat, List.rev !lin_vars
  in
  let newpat,lin_vars = wrap oldpat in
  newpat, lin_vars

let add_setslots es expr =
  let insert_expr (linvar, newvar) =
    app (* ~loc:oldpat.ppat_loc *) (setfunc_raw ()) [Exp.ident ~loc:linvar.loc linvar; evar ~loc:linvar.loc newvar]
  in
  List.fold_right (fun e expr ->
      app
        (monad_bind_raw ())
        [insert_expr e; lam (punit ()) expr]) es expr

let add_getslots es expr =
  List.fold_right (fun (v,slot) expr ->
      app
        (monad_bind_raw ())
        [app (getfunc ()) [slot];
         lam (pvar v) expr]) es expr

let rec linval ({pexp_desc;pexp_loc;pexp_attributes} as outer) =
  match pexp_desc with
  | Pexp_ident _ | Pexp_constant _ 
  | Pexp_construct (_,None) 
  | Pexp_variant (_,None) ->
     outer, []
    
  | Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident"!!"}} , [(Nolabel,exp)]) ->
     let newvar = newname "linval" in
     constr ~loc:pexp_loc "L" [longident ~loc:pexp_loc newvar], [(newvar,exp)]
     
  | Pexp_tuple (exprs) ->
    let exprs, bindings = List.split (List.map linval exprs) in
    {pexp_desc=Pexp_tuple(exprs);pexp_loc;pexp_attributes}, List.concat bindings

  | Pexp_construct ({txt=Lident "Data"},Some(expr)) ->
     constr ~loc:pexp_loc ~attrs:pexp_attributes "L" [expr], []
       
  | Pexp_construct (lid,Some(expr)) ->
     let expr, binding = linval expr in
     {pexp_desc=Pexp_construct(lid,Some(expr));pexp_loc;pexp_attributes}, binding
  | Pexp_variant (lab,Some(expr)) ->
     let expr, binding = linval expr in
     {pexp_desc=Pexp_variant(lab,Some(expr));pexp_loc;pexp_attributes}, binding
  | Pexp_record (fields,expropt) ->
     let fields, bindings =
       List.split (List.map (fun (lid,expr) -> let e,b = linval expr in (lid,e),b) fields)
     in
     let bindings = List.concat bindings in
     let expropt, bindings =
       match expropt with
       | Some expr ->
          let expr, binding = linval expr in
          Some expr, binding @ bindings
       | None -> None, bindings
     in
     {pexp_desc=Pexp_record(fields,expropt);pexp_loc;pexp_attributes}, bindings
  | Pexp_array (exprs) ->
     let exprs, bindings =
       List.split (List.map linval exprs)
     in
     {pexp_desc=Pexp_array(exprs);pexp_loc;pexp_attributes}, List.concat bindings
  | Pexp_constraint (expr,typ) ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_constraint(expr,typ);pexp_loc;pexp_attributes}, binding
  | Pexp_coerce (expr,typopt,typ) ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_coerce(expr,typopt,typ);pexp_loc;pexp_attributes}, binding
  | Pexp_lazy expr ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_lazy(expr);pexp_loc;pexp_attributes}, binding
  | Pexp_open (oflag,lid,expr) ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_open(oflag,lid,expr);pexp_loc;pexp_attributes}, binding
  | Pexp_apply (expr,exprs) ->
     let expr, binding = linval expr in
     let exprs, bindings =
       List.split @@
         List.map
           (fun (lab,expr) -> let expr,binding = linval expr in (lab,expr),binding)
           exprs
     in
     begin match binding @ List.concat bindings with
     | [] -> {pexp_desc=Pexp_apply(expr,exprs);pexp_loc;pexp_attributes}, []
     | _ ->
        error pexp_loc "function call inside %linval cannot contain slot references (!! slotname)"
     end
  | Pexp_object ({pcstr_self={ppat_desc=Ppat_any}; pcstr_fields=fields} as o) ->
     let new_fields, bindings =
       List.split @@ List.map
         (function
          | ({pcf_desc=Pcf_method (name,Public,Cfk_concrete(fl,expr))} as f) ->
             let new_expr, binding = linval expr in
             {f with pcf_desc=Pcf_method(name,Public,Cfk_concrete(fl,new_expr))}, binding
          | _ ->
             error pexp_loc "object can only contain public method")
         fields
     in
     {pexp_desc=Pexp_object({o with pcstr_fields=new_fields});pexp_loc;pexp_attributes},
     List.concat bindings
  | Pexp_object _ ->
     failwith "object in linval can't refer to itself"
  | Pexp_poly (expr,None) ->
     let expr, binding = linval expr in
     {pexp_desc=Pexp_poly(expr,None);pexp_loc;pexp_attributes}, binding
  | Pexp_poly (expr,_) ->
     failwith "object method can not have type ascription"
  | Pexp_let (_,_,_) | Pexp_function _
  | Pexp_fun (_,_,_,_) | Pexp_match (_,_) | Pexp_try (_,_)
  | Pexp_field (_,_) | Pexp_setfield (_,_,_) | Pexp_ifthenelse (_,_,_)
  | Pexp_sequence (_,_) | Pexp_while (_,_) | Pexp_for (_,_,_,_,_)
  | Pexp_send (_,_) | Pexp_new _ | Pexp_setinstvar (_,_) | Pexp_override _
  | Pexp_letmodule (_,_,_) | Pexp_assert _ | Pexp_newtype (_,_)
  | Pexp_pack _ | Pexp_extension _
  | Pexp_unreachable | Pexp_letexception _
    -> failwith "%linval can only contain values"

let bindings_of_let bindings =
  List.mapi (fun i binding ->
      let var = newname "let" in
      {binding with pvb_pat = pvar var}, (var, binding.pvb_pat)
    ) bindings

(* [p0 = ??] and [p1 = ??] and .. and e ==> [bind dum$0 (fun p0 -> bind dum$1 (fun p1 -> .. -> e))] *)
let bindbody_of_let exploc bindings exp =
  let rec make i bindings =
    match bindings with
    | [] -> exp
    | (binding,(var,origpat)) :: t ->
      let name = evar var [@metaloc binding.pvb_expr.pexp_loc] in
      let f = [%expr (fun [%p origpat] -> [%e make (i+1) t])] [@metaloc binding.pvb_loc] in
      let new_exp = [%expr [%e monad_bind ()] [%e name] [%e f]] [@metaloc exploc] in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  make 0 bindings

(* (\* [{lab1} = e1] and [{lab2} = e2 and .. and e ==> e1 ~bindto:lab1 >>= (fun () -> e2 ~bindto:lab2 ]  *\) *)
(* let slot_bind bindings expr = *)
(*   let f binding expr = *)
(*     match binding with *)
(*     | {pvb_pat = {ppat_desc = Ppat_record ([({txt},_)],Closed)}; pvb_expr = rhs} *)
(*     | {pvb_pat = {ppat_desc = Ppat_type {txt}}; pvb_expr = rhs} -> *)
(*       let lensname = String.concat "." (Longident.flatten txt) in *)
(*       let f = Exp.fun_ Label.nolabel None (punit ()) expr in *)
(*       [%expr [%e monad_bind ()] ([%e rhs] ~bindto:[%e evar lensname]) [%e f]] *)
(*     | _ -> raise Not_found *)
(*   in List.fold_right f bindings expr *)
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
         let protocol_var = newname "match_p" in
         let polarity_var = newname "match_q" in
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
  [%type: [%t (variant rows Closed None)] * [%t any () ] -> [%t any () ] ]

     
let expression_mapper id mapper exp attrs =
  let pexp_attributes = exp.pexp_attributes @ attrs in
  let pexp_loc=exp.pexp_loc in
  let process_inner expr = mapper.Ast_mapper.expr mapper expr
  in
  match id, exp.pexp_desc with

  | "lin", Pexp_let (Nonrecursive, vbls, expr) ->
     let lin_binding ({pvb_pat;pvb_expr} as vb) =
         let newpat, inserts = lin_pattern pvb_pat in
         {vb with pvb_pat=newpat}, inserts
     in
     let new_vbls, inserts = List.split (List.map lin_binding vbls) in
     let new_expr = add_setslots (List.concat inserts) expr in
     let make_bind {pvb_pat;pvb_expr;pvb_loc;pvb_attributes} expr =
       app ~loc:pexp_loc (monad_bind ()) [pvb_expr; app ~loc:pvb_loc (mkbindfun ()) [lam ~loc:pvb_loc pvb_pat expr]]
     in
     let expression = List.fold_right make_bind new_vbls new_expr
     in
     Some (process_inner expression)

  | "lin", Pexp_match(matched, cases) ->
     let lin_match ({pc_lhs=pat;pc_rhs=expr} as case) =
       let newpat, inserts = lin_pattern pat in
       let newexpr = add_setslots inserts expr in
       {case with pc_lhs=newpat;pc_rhs=newexpr}
     in
     let cases = List.map lin_match cases in
     let new_exp =
       app ~loc:pexp_loc ~attrs:pexp_attributes
         (monad_bind_raw ())
         [matched;
          Exp.function_ ~loc:pexp_loc cases]
     in
     Some (process_inner new_exp)

  | "lin", Pexp_function(cases) ->
     let lin_match ({pc_lhs=pat;pc_rhs=expr} as case) =
       let newpat, inserts = lin_pattern pat in
       let newexpr = add_setslots inserts expr in
       {case with pc_lhs=newpat;pc_rhs=newexpr}
     in
     let cases = List.map lin_match cases in
     Some (app (mkbindfun ()) [process_inner {pexp_desc=Pexp_function(cases); pexp_loc; pexp_attributes}])
     
  | "lin", Pexp_fun(Nolabel,None,pat,expr) ->
     let newpat, inserts = lin_pattern pat in
     let newexpr = add_setslots inserts expr in
     Some (app (mkbindfun ()) [process_inner {pexp_desc=Pexp_fun(Nolabel,None,newpat,newexpr); pexp_loc; pexp_attributes}])
     
  | "lin", _ ->
     error pexp_loc "Invalid content for extension %lin; it must be \"let%lin slotname = ..\" OR \"match%lin slotname with ..\""

  | "linret", expr ->
     let new_exp,bindings = linval {pexp_desc=expr;pexp_loc;pexp_attributes} in
     let new_exp = constr ~loc:pexp_loc "L" [new_exp] in
     let new_exp = app (monad_return_raw ()) [new_exp] in
     let new_exp = add_getslots bindings new_exp in
     Some(new_exp)

  (* monadic bind *)
  (* let%s p = e1 in e2 ==> let dum$0 = e1 in Session.(>>=) dum$0 e2 *)
  | ("s"|"w"), Pexp_let (Nonrecursive, vbl, expression) ->
     let new_exp =
       let vbl = bindings_of_let vbl in
       Exp.let_
         Nonrecursive
         (fst @@ List.split vbl)
         (bindbody_of_let exp.pexp_loc vbl expression)
      in
      Some (mapper.Ast_mapper.expr mapper { new_exp with pexp_attributes })
  | ("s"|"w"), _ -> error pexp_loc "Invalid content for extension %s|%w"

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

let mapper_fun _ =
  let open Ast_mapper in
  let rec expr mapper outer =
  match outer with
  | {pexp_desc=Pexp_extension ({ txt = id }, PStr([{pstr_desc=Pstr_eval(inner,inner_attrs)}])); pexp_attributes=outer_attrs} ->
     begin match expression_mapper id mapper inner (inner_attrs @ outer_attrs) with
     | Some exp -> exp
     | None -> default_mapper.expr mapper outer
     end
  | _ -> default_mapper.expr mapper outer
  in
  {default_mapper with expr}


(* let expression_mapper id mapper exp attrs = *)
(*   let pexp_attributes = attrs @ exp.pexp_attributes in *)
(*   let pexp_loc=exp.pexp_loc in *)
(*   match id, exp.pexp_desc with *)

(*   (\* slot bind *\) *)
(*   (\* let%lin {lab} = e1 in e2 ==> Session.(>>=) (e1 ~bindto:lab) (fun () -> e2) *\) *)
(*   | "lin", Pexp_let (Nonrecursive, vbl, expression) -> *)
(*       let new_exp = slot_bind vbl expression in *)
(*       Some (mapper.Ast_mapper.expr mapper { new_exp with pexp_attributes }) *)
(*   | "lin", _ -> error pexp_loc "Invalid content for extension %lin" *)

(*   | _ -> None *)

(* let rebind_module modexpr = *)
(*   match modexpr.pmod_desc with *)
(*   | Pmod_ident {txt = id} -> root_module := String.concat "." (Longident.flatten id) *)
(*   | _ -> error modexpr.pmod_loc "Use (module M) here." *)
  
(* let runner ({ ptype_loc = loc } as type_decl) = *)
(*   match type_decl with *)
(*   (\* | {ptype_kind = Ptype_record labels} -> *\) *)
(*   | {ptype_name = {txt = name}; ptype_manifest = Some ({ptyp_desc = Ptyp_object (labels, Closed)})} -> *)
(*     let obj =  *)
(*       let meth (fname,_,_) = *)
(*         {pcf_desc = *)
(*            Pcf_method ({txt=fname;loc=Location.none}, *)
(*                        Public, *)
(*                        Cfk_concrete(Fresh, [%expr Session.Empty])); *)
(*          pcf_loc = Location.none; *)
(*          pcf_attributes = []} *)
(*       in *)
(*       Exp.object_ {pcstr_self = Pat.any (); pcstr_fields = List.map meth labels} *)
(*     in *)
(*     let mkfun = Exp.fun_ Label.nolabel None in *)
(*     let runner = mkfun (pvar "x") (app [%expr Session._run_internal] [obj; evar "x"]) in *)
(*     let quoter = Ppx_deriving.create_quoter () in *)
(*     let varname = "run_" ^ name in *)
(*     [{pstr_desc = Pstr_value (Nonrecursive, [Vb.mk (pvar varname) (Ppx_deriving.sanitize ~quoter runner)]); pstr_loc = Location.none}] *)
(*   | _ -> error loc "run_* can be derived only for record or closed object types"  *)

(* let has_runner attrs = *)
(*   List.exists (fun ({txt = name},_) -> name = "runner")  attrs *)
       
(* let mapper_fun _ = *)
(*   let open Ast_mapper in *)
(*   let expr mapper outer = *)
(*   match outer.pexp_desc with *)
(*   | Pexp_extension ({ txt = id }, PStr [{ pstr_desc = Pstr_eval (inner, attrs) }]) -> *)
(*      begin match expression_mapper id mapper inner attrs with *)
(*      | Some exp -> exp *)
(*      | None -> default_mapper.expr mapper outer *)
(*      end *)
(*   | _ -> default_mapper.expr mapper outer *)
(*   and stritem mapper outer = *)
(*     match outer with *)
(*     | {pstr_desc = Pstr_extension (({ txt = "s_syntax_rebind" }, PStr [{ pstr_desc = Pstr_eval ({pexp_desc=Pexp_pack modexpr}, _) }]),_) }-> *)
(*        rebind_module modexpr; *)
(*        [{outer with pstr_desc = Pstr_eval ([%expr ()],[])}] (\* replace with () *\) *)
(*     | {pstr_desc = Pstr_type (_, type_decls)} -> *)
(*        let runners = *)
(*          List.map (fun type_decl -> *)
(*            if has_runner type_decl.ptype_attributes then *)
(*              [runner type_decl] *)
(*            else []) type_decls *)
(*        in [outer] @ List.flatten (List.flatten runners) *)
(*     | _ -> [default_mapper.structure_item mapper outer] *)
(*   in *)
(*   let structure mapper str = *)
(*     List.flatten (List.map (stritem mapper) str) *)
(*   in *)
(*   {default_mapper with expr; structure} *)
