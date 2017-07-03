open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "lens"
let raise_errorf = Ppx_deriving.raise_errorf

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

let rec traverse f ({ptyp_desc; ptyp_loc = loc} as typ) =
  let f ptyp_desc =
    match ptyp_desc with
    | Ptyp_var var -> Ptyp_var (f var)
    | Ptyp_alias(t, var) -> Ptyp_alias(traverse f t, f var)
    | Ptyp_any -> Ptyp_any
    | Ptyp_arrow(lab,t1,t2) -> Ptyp_arrow(lab, traverse f t1, traverse f t2)
    | Ptyp_tuple ts -> Ptyp_tuple (List.map (traverse f) ts)
    | Ptyp_constr(lab,ts) -> Ptyp_constr(lab, List.map (traverse f) ts)
    | Ptyp_object(flds,flg) -> Ptyp_object(List.map (fun (str,attr,t) -> (str,attr,traverse f t)) flds, flg)
    | Ptyp_class(name,ts) -> Ptyp_class(name, List.map (traverse f) ts)
    | Ptyp_poly(_, _) | Ptyp_variant(_,_,_)
    | Ptyp_package _ | Ptyp_extension _ ->
       raise_errorf ~loc "%s cannot handle a type in a field" deriver
  in
  {typ with ptyp_desc = f ptyp_desc}

let free_tvars typ =
  let rvars = ref [] in
  let f var = rvars := var::!rvars; var in
  ignore @@ traverse f typ;
  !rvars

let rename_tvars mapping typ =
  let f var =
    try List.assoc var mapping with Not_found -> var
  in
  traverse f typ

(* rename tvars in typ with fresh name *)
let change_tvars tvars typ =
  let mapping = ref [] in
  let rec fresh var =
    if List.exists (fun v->v=var) tvars then
      fresh (var^var)
    else 
      var
  in
  let rename var =
    if List.exists (fun v->v=var) tvars then
      try
        List.assoc var !mapping
      with Not_found ->
        begin
          let newvar = fresh var in
          mapping := (var,newvar)::!mapping;
          newvar
        end
    else begin
      mapping := (var,var)::!mapping;
      var
    end
  in
  !mapping, traverse rename typ

let lens_typ rtyp ftyp =
  let getter_typ = Typ.arrow Label.nolabel rtyp ftyp
  in
  let vars = free_tvars getter_typ in
  let mapping, setter_2ndarg = change_tvars vars ftyp in
  let setter_result = rename_tvars mapping rtyp in
  let setter_typ = Typ.arrow Label.nolabel rtyp (Typ.arrow Label.nolabel setter_2ndarg setter_result) in
  Typ.tuple [getter_typ; setter_typ]

let object_update obj labels fields =
  let meth (fname,_,_) =
    let expr = 
      try
        List.assoc fname fields
      with Not_found ->
        Exp.send obj fname
    in        
    {pcf_desc =
       Pcf_method ({txt=fname;loc=Location.none},
                   Public,
                   Cfk_concrete(Fresh,expr));
     pcf_loc = Location.none;
     pcf_attributes = []}
  in
  Exp.object_ {pcstr_self = Pat.any (); pcstr_fields = List.map meth labels}

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  parse_options options;
  let quoter = Ppx_deriving.create_quoter () in
  match type_decl with
  | {ptype_kind = Ptype_record labels} ->
    let typename = Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl in
    let fn = Exp.fun_ Label.nolabel None in
    let getter field =
      fn (pvar typename) (Exp.field (evar typename) (lid field))
    and setter field = 
      fn (pvar typename) (fn (pvar field) (record ~over:(evar typename) [(field, (evar field))]))
    in
    let typ = Ppx_deriving.core_type_of_type_decl type_decl in
    let lens { pld_name = { txt = name }; pld_type } =
      Vb.mk (Pat.constraint_ (pvar name) (lens_typ typ pld_type))
            (Ppx_deriving.sanitize ~quoter (tuple [getter name; setter name]))
    in
    List.map lens labels
  | {ptype_manifest = Some ({ptyp_desc = Ptyp_object (labels, Closed)} as typ)} ->
    let typename = Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl in
    let fn = Exp.fun_ Label.nolabel None in
    let getter field =
      fn (pvar typename) (Exp.send (evar typename) field)
    and setter field = 
      fn (pvar typename) (fn (pvar field) (object_update (evar typename) labels [(field, (evar field))]))
    in
    let lens (field,_,ftyp) =
      Vb.mk (Pat.constraint_ (pvar field) (lens_typ typ ftyp))
            (Ppx_deriving.sanitize ~quoter (tuple [getter field; setter field]))
    in
    List.map lens labels
  | _ -> raise_errorf ~loc "%s can be derived only for record or closed object types" deriver

let wrap_predef_option typ =
  typ

let sig_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  parse_options options;  
  match type_decl with
  | {ptype_kind = Ptype_record labels} ->
    let typ = Ppx_deriving.core_type_of_type_decl type_decl in
    let lens { pld_name = { txt = name }; pld_type } =
      Sig.value (Val.mk (mknoloc name) (lens_typ typ pld_type))
    in
    List.map lens labels
  | {ptype_manifest = Some ({ptyp_desc = Ptyp_object (labels, Closed)} as typ)} ->
    let lens (field,_,ftyp) =
      Sig.value (Val.mk (mknoloc field) (lens_typ typ ftyp))
    in
    List.map lens labels
  | _ -> raise_errorf ~loc "%s can only be derived for record types" deriver

let () =
  Ppx_deriving.(register (create deriver
    ~type_decl_str: (fun ~options ~path type_decls ->
       [Str.value Nonrecursive (List.concat (List.map (str_of_type ~options ~path) type_decls))])
    ~type_decl_sig: (fun ~options ~path type_decls ->
       List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
