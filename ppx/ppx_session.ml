(* open Printf *)
(* open Ast_mapper *)
(* open Ast_helper *)
open Asttypes
open Parsetree
(* open Longident *)
(* open Ast_convenience *)

let newname i =
  Printf.sprintf "__ppx_session_%d" i

let monad_bind =
  [%expr [%e Ast_helper.Exp.ident (Ast_convenience.lid ("Session.>>="))]]

               
(* [?? = e0] and [?? = e1] and .. ==> [dum$0 = e0] and [dum$1 = e1] and .. 
   (to use with bindbody_of_let.) *)
let bindings_of_let bindings =
  List.mapi (fun i binding ->
      {binding with pvb_pat = Ast_convenience.pvar (newname i)}
    ) bindings

(* [p0 = ??] and [p1 = ??] and .. and e ==> [bind dum$0 (fun p0 -> bind dum$1 (fun p1 -> (.. e)))] *)
let bindbody_of_let exploc bindings exp =
  let rec make i bindings =
    match bindings with
    | [] -> exp
    | binding :: t ->
      let name = (Ast_convenience.evar (newname i)) [@metaloc binding.pvb_expr.pexp_loc] in
      let f = [%expr (fun [%p binding.pvb_pat] -> [%e make i t])] [@metaloc binding.pvb_loc] in
      let new_exp = [%expr [%e monad_bind] [%e name] [%e f]] [@metaloc exploc] in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  make 0 bindings
   
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
  | "s", _ -> Location.raise_errorf ~loc:pexp_loc "Invalid content for extension %%s"
      
  (* [%select0 `labl] ==> Session0._select (fun x -> `labl(x)) *)
  | "select0", Pexp_variant (labl, None) ->
     let new_exp =
       [%expr Session0._select
              (fun [%p Ast_convenience.pvar "x"] ->
                [%e {pexp_desc=Pexp_variant (labl, Some(Ast_convenience.evar "x")); pexp_loc; pexp_attributes } ])]
     in
     Some (mapper.expr mapper {new_exp with pexp_attributes})
  | "select0", _ -> Location.raise_errorf ~loc:pexp_loc "Invalid content for extension %%select0"
     
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
  (* | "branch0", Pexp_match (exp, cases) -> *)
     
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

