(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types
module S = Ast_tostring

(* ------------------------------------------------------------------------- *)

(* Takes a minus AST0 and extracts all metavariables used in the rule.
 *
 * In general, the metavariable layout is
 * (type, (inherit_rule, metaname), constraints)
 * e.g. parameter list[rule1.n] P is ("parameter list[rule1.n]", ("","P"), "")
 * and position free.p1!=loop.ok is ("position", ("free", "p1"), "!=loop.ok")
 *
 * NOTE: inherit_rule is only for inherited rules, ie. akin to "rulename.mv".
 * If the metavariable is in local scope, inherit_rule will be "".
 *
 * Named arguments in here:
 *  - rn is the rulename (string)
 *  - mc is an Ast0.mcode ('a mcode)
 *  - mn is an Ast.meta_name (type alias for (rule_name, metavar_name) tuple)
 *  - typ/before is the type (string), put in the type spot
 *  - constr/after is the constraint (string), put in the constraints spot
 *  - listlen is a list_len (Ast.list_len)
 *)

(* ------------------------------------------------------------------------- *)
(* TYPES AND HELPERS *)

type meta_variable = string * (string * string) * string

let make_mv typ (rule, name) constr = (typ, (rule,name), constr)

(* If the externally specified rulename is the same as the internal rulename
 * (ie. the one attached to the metavariable) OR the external rulename is
 * nameless ("rule starting on line ..."), then there is no inheritance
 * required; ie. return true.
 *)
let no_inherit extern intern =
  extern = intern || String.contains extern ' '

let name_str ~rn (r, mn) =
  if no_inherit r rn
  then mn
  else r ^ "." ^ mn

let name_tup ~rn (r, mn) =
  if no_inherit r rn
  then ("", mn)
  else (r, mn)

let str_tup str = ("", str)

let tostring_mv (t, rnm, c) =
  let full_name = name_str ~rn:"" rnm in
  String.concat "" [t; full_name; c]

(* the type used and returned by the visitor.
 * we're using a set to eliminate duplicates, since a metavariable might
 * appear several times in a rule, but we only want it declared once.
 *)
module MVSet = Set.Make(
  struct
    type t = meta_variable

    (* we use the normal string comparison, except if the string starts with
     * "type", "typedef" or "identifier" in which case it comes before others.
     * This is to ensure that types and identifiers get printed first since
     * other metavariables might be dependent on them.
     *)
    let compare (t1,(_,n1),_) (t2,(_,n2),_) =

      let starts_with s c = Str.string_match (Str.regexp ("^"^s)) c 0 in
      let is_type = starts_with "type" in
      let is_identifier = starts_with "identifier" in

      match (is_type t1, is_type t2) with
        | true, false -> -1
        | false, true -> 1
        | true, true -> String.compare n1 n2
        | false, false ->
            (match (is_identifier t1, is_identifier t2) with
              | true, false -> -1
              | false, true -> 1
              | _ -> String.compare n1 n2
            )
  end
)

(* ------------------------------------------------------------------------- *)
(* STRING HELPERS *)

(* These functions take subcomponents of the AST0 and turn them into
 * pretty strings for printing.
 * ALL FUNCTIONS HERE RETURN STRINGS
 *)

(* get string formatted version of type (used as front of meta expressions) *)
let type_c ~form =
  (* TODO: figure out when default and prefix are used ... *)
  let (default, prefix) =
    match form with
    | Ast.ANY -> ("expression ", "")
    | Ast.ID -> ("idexpression ", "idexpression ")
    | Ast.LocalID -> ("local idexpression ", "local idexpression ")
    | Ast.GlobalID -> ("global idexpression ", "global idexpression ")
    | Ast.CONST -> ("constant ", "constant ") in
  let type2c a =
    match Ast.string_of_fullType a with
    | "unknown *" -> default ^ " *"
    | a -> prefix ^ a in
  function
    | Some [a] -> type2c a
    | Some a -> prefix ^ ("{" ^ (String.concat "," (List.map type2c a)) ^ "} ")
    | None -> default

(* TODO: in SeedId, we sometimes (?) want to keep the rulename; but not if it
 * has been declared before?
 *)
let seed ~rn =
  let se = function
    | Ast.SeedString s -> "\"" ^ s ^ "\"" | Ast.SeedId (r,nm) -> nm in
  function
    | Ast.NoVal -> ""
    | Ast.StringSeed s -> " = \"" ^ s ^ "\""
    | Ast.ListSeed s -> " = " ^ (String.concat " ## " (List.map se s))
    | Ast.ScriptSeed (_, lang, params, _pos, code) ->
        Printf.sprintf " = script:%s (%s) {%s}"
          lang
          (String.concat "," (List.map (fun (nm,_) -> name_str ~rn nm) params))
          code

let string_of_operator_constraint cstr =
  match cstr with
    Ast.CstrAssignOp op -> Ast.string_of_assignOp op
  | Ast.CstrBinaryOp op -> Ast.string_of_binaryOp op

let rec constraints_to_buffer ~rn buffer cstr =
  let string_of_expression e =
    match Ast0.unwrap e with
      Ast0.Ident id ->
	begin
	  match Ast0.unwrap id with
	    Ast0.Id id -> Ast0.unwrap_mcode id
	  | _ -> failwith "string_of_expression"
	end
    | _ -> failwith "string_of_expression" in
  let print_sub op first item =
    match item with
      Ast.CstrTrue | Ast.CstrFalse -> first
    | _ ->
	if not first then
	  Buffer.add_string buffer op;
	Buffer.add_string buffer "(";
	constraints_to_buffer ~rn buffer item;
	Buffer.add_string buffer ")";
	false in
  let simple_item item =
    match item with
      Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntEq i)) ->  i
    | Ast.CstrConstant (Ast.CstrString s) -> s
    | Ast.CstrOperator item -> string_of_operator_constraint item
    | Ast.CstrMeta_name mn -> name_str ~rn mn
    | Ast.CstrExpr e -> string_of_expression e
    | _ -> raise Exit in
  match cstr with
    Ast.CstrTrue | Ast.CstrSub [] -> ()
  | Ast.CstrFalse ->
      failwith "Ast_cocci.string_of_constraints: no syntax for Ast.CstrFalse"
  | Ast.CstrAnd list -> ignore (List.fold_left (print_sub " && ") true list)
  | Ast.CstrOr list ->
      begin
	match try Some (List.map simple_item list) with Exit -> None with
	  Some [singleton] ->
	    Printf.bprintf buffer "= %s" singleton;
	| Some (first :: others) ->
	    Printf.bprintf buffer "= {%s" first;
	    List.iter (fun s -> Printf.bprintf buffer ",%s" s) others;
	    Buffer.add_string buffer "}"
	| _ -> ignore (List.fold_left (print_sub " || ") true list)
      end
  | Ast.CstrNot item ->
      Buffer.add_string buffer "!(";
      constraints_to_buffer ~rn buffer item;
      Buffer.add_string buffer ")"
  | Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntEq i)) ->
      Printf.bprintf buffer "= %s" i
  | Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntLeq i)) ->
      Printf.bprintf buffer "<= %d" i
  | Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntGeq i)) ->
      Printf.bprintf buffer ">= %d" i
  | Ast.CstrConstant (Ast.CstrString s) -> Printf.bprintf buffer "= %s" s
  | Ast.CstrOperator item ->
      Printf.bprintf buffer "= %s" (string_of_operator_constraint item)
  | Ast.CstrMeta_name mn -> Printf.bprintf buffer "= %s" (name_str ~rn mn)
  | Ast.CstrRegexp (s, _) -> Printf.bprintf buffer "=~ \"%s\"" s
  | Ast.CstrScript (_,(_, lang, params, _pos, code)) ->
      Printf.bprintf buffer ": script:%s (%s) {%s}" lang
	(String.concat "," (List.map (fun (nm,_) -> name_str ~rn nm) params))
	code
  | Ast.CstrExpr e -> Buffer.add_string buffer (string_of_expression e)
  | Ast.CstrSub (first :: others) ->
      Printf.bprintf buffer "<= {%s" (name_str ~rn first);
      List.iter (fun mn -> Printf.bprintf buffer ",%s" (name_str ~rn mn))
	others;
      Buffer.add_string buffer "}"
  | Ast.CstrType ty -> failwith "string_of_constraints: type"

let constraints ~rn cstr =
  let buffer = Buffer.create 17 in
  constraints_to_buffer ~rn buffer cstr;
  let result = Buffer.contents buffer in
  if result = "" then ""
  else " " ^ result

let list_len ~rn = function
  | Ast0.AnyListLen -> " "
  | Ast0.MetaListLen ((mn,_,_,_,_,_),_) -> "[" ^ (name_str ~rn mn) ^ "] "
  | Ast0.CstListLen i -> "[" ^ (string_of_int i) ^ "] "


(* ------------------------------------------------------------------------- *)
(* MSET HELPERS *)

(* These functions take subcomponents of the AST0 and turn them into MVSets of
 * prettily formatted strings, ready to be bound in the combiner.
 * ALL FUNCTIONS HERE RETURN META_VARIABLE MVSET.T
 *)

(* Scours an optional list of type_c's for meta types and meta identifiers
 * used in the types. If the type_c's have dependencies to other metavariables,
 * we need to declare those metavariables as well of course.
 *)
let types ~rn = function
  | Some typecs ->
      (* TODO: are the keep_bindings used for anything ? *)
      let bin = function
          Ast.Unitary -> ""
        | Ast.Nonunitary -> ""
        | Ast.Saved -> "" in
      let get_meta_id acc ty =
        match Ast_cocci.unwrap ty with
          Ast_cocci.MetaId(mn, _, b, _) ->
            let mn' = Ast_cocci.unwrap_mcode mn in
            let metavar = make_mv "identifier" (name_tup ~rn mn') (bin b) in
            MVSet.add metavar acc
        | _ -> acc in
      let rec get_meta_type acc ty =
        match Ast_cocci.unwrap ty with
          Ast.MetaType(mn, _, b, _) ->
            let mn' = Ast_cocci.unwrap_mcode mn in
            let metavar = make_mv "type " (name_tup ~rn mn') (bin b) in
            MVSet.add metavar acc
        | Ast.TypeName s ->
            let metavar = ("typedef ", str_tup (Ast.unwrap_mcode s), "") in
            MVSet.add metavar acc
        | Ast.Decimal(_, _, nm1, _, nm2, _) ->
            let get_meta_id_opt v o = Common.default v (get_meta_id v) o in
            let acc =
              get_meta_id_opt acc (Ast_cocci.ident_of_expression_opt nm1) in
            let acc =
              get_meta_id_opt acc
                (Common.default None Ast_cocci.ident_of_expression_opt nm2) in
            acc
        | Ast.EnumName (_, Some n)
        | Ast.StructUnionName(_, Some n) -> get_meta_id acc n
        | Ast.SignedT (_, Some t) -> get_meta_type acc t
        | Ast.Pointer (t, _)
        | Ast.Array (t, _, _, _) -> get_meta_type_full acc t
	| Ast.TypeOfExpr(_,_,exp,_) -> acc (* not sure *)
	| Ast.TypeOfType(_,_,ty,_) -> get_meta_type_full acc ty
        | _ -> acc
      and get_meta_type_full acc ty =
        get_meta_type acc (Common.just (Ast_cocci.typeC_of_fullType_opt ty)) in
      List.fold_left get_meta_type_full MVSet.empty typecs
  | None -> MVSet.empty

(* Function to call on mcodes. We are only interested in the mcodes because of
 * the positions/metavars that might be attached to them.
 * Note that attached metavariables might have metavariables attached to
 * themselves as well!
 *)
let mcode ~rn ~mc:(_,_,_,_,pos,_) =
  let rec add_one_pos set =

    (* adds the mn metavar + any metapositions attached to it to the set. *)
    let handle_metavar ~typ ~mn ~positions ~set =
      let mv = make_mv typ (name_tup ~rn mn) "" in
      let added_mv_set = MVSet.add mv set in
      MVSet.union added_mv_set (add_all_pos positions) in

    (* extracting the node is equivalent to calling Ast0.unwrap *)
    function
    | Ast0.ExprTag {Ast0.node = Ast0.MetaExpr((mn,_,_,_,p,_),_,_,_,_,_); _} ->
        handle_metavar ~typ:"expression " ~mn ~positions:!p ~set
    | Ast0.StmtTag {Ast0.node = Ast0.MetaStmt((mn,_,_,_,p,_),_,_); _} ->
        handle_metavar ~typ:"statement " ~mn ~positions:!p ~set
    | Ast0.DeclTag {Ast0.node = Ast0.MetaDecl((mn,_,_,_,p,_),_,_); _} ->
        handle_metavar ~typ:"declaration " ~mn ~positions:!p ~set
    | Ast0.IdentTag {Ast0.node = Ast0.MetaId((mn,_,_,_,p,_),_,_,_); _} ->
        handle_metavar ~typ:"identifier " ~mn ~positions:!p ~set
    | Ast0.TypeCTag {Ast0.node = Ast0.MetaType((mn,_,_,_,p,_),_,_); _} ->
        handle_metavar ~typ:"type " ~mn ~positions:!p ~set
    | Ast0.MetaPosTag(Ast0.MetaPos((mn,_,_,_,_,_), mns, colt)) ->
        let constr = constraints ~rn mns in
        let collect = (match colt with Ast.PER -> "" | Ast.ALL -> " any") in
        let pos = make_mv "position " (name_tup ~rn mn) (constr ^ collect) in
        MVSet.add pos set
    | Ast0.MetaPosTag(Ast0.MetaCom((mn,_,_,_,_,_),_)) ->
        let com = make_mv "comments " (name_tup ~rn mn) "" in
        MVSet.add com set
    | _ -> failwith "should only have metavariables in here."

  and add_all_pos lst = List.fold_left add_one_pos MVSet.empty lst in
  add_all_pos !pos

(* turns mcode into MVSet of formatted strings.
 * (mc : 'a mcode) is the mcode,
 * (totup_fn : 'a -> string * string) formats the mcode value.
 *)
let mc_format ~rn ~mc:((mn,_,_,_,_,_) as mc) ~totup_fn ~before ~after =
  let pos = mcode ~rn ~mc in
  let mv = make_mv before (totup_fn mn) after in
  MVSet.add mv pos

let as_format a b afn bfn = failwith "\"as\" metavariables not supported"

(* turns meta_name mcode with list information into MVSet of formatted strings.
 * (mc : Ast.meta_name mcode) becomes <before mc[ll_output]>.
 *)
let list_format ~rn ~before ~mc:((mn,_,_,_,_,_) as mc) ~listlen =
  let pos = mcode ~rn ~mc in
  let mvname = name_tup ~rn mn in
  let mv = make_mv (before ^ (list_len ~rn listlen)) mvname "" in
  MVSet.add mv pos

(* for iterators and declarers *)
let ids ~rn ~typ ~id =
  match Ast0.unwrap id with
  | Ast0.Id mc ->
      mc_format ~rn ~mc ~totup_fn:str_tup ~before:(typ ^ " name ") ~after:""
  | Ast0.MetaId (mc, constr, s, _) -> (* ever seed here? *)
      let constr' = constraints ~rn constr in
      let totup_fn = name_tup ~rn in
      mc_format ~rn ~mc ~totup_fn ~before:(typ ^ " ") ~after:constr'
  | _ -> failwith (typ ^ " with non-(Id/MetaId). dunno what this means")


(* ------------------------------------------------------------------------- *)
(* THE COMBINER *)

(* MVSet Visitor_ast0_types.combiner_rec_functions
 * Using the flat combiner from Visitor_ast0
 *)

let metavar_combiner rn =
  let option_default = MVSet.empty in
  let bind x y = MVSet.union x y in

  (* the mcodes might contain positions which should be declared as metavars *)
  let mcode mc = mcode ~rn ~mc in
  let meta_mcode a = failwith ("NOT ALLOWED") in (* should be handled before *)
  let string_mcode = mcode in
  let const_mcode = mcode in
  let simpleAssign_mcode = mcode in
  let opAssign_mcode = mcode in
  let fix_mcode = mcode in
  let unary_mcode = mcode in
  let arithOp_mcode = mcode in
  let logicalOp_mcode = mcode in
  let cv_mcode = mcode in
  let sign_mcode = mcode in
  let struct_mcode = mcode in
  let storage_mcode = mcode in
  let inc_mcode = mcode in

  (* apply the passed function, do nothing else *)
  let donothing c fn v = fn v in
  let dotsexprfn = donothing in
  let dotsinitfn = donothing in
  let dotsparamfn = donothing in
  let dotsstmtfn = donothing in
  let dotsdeclfn = donothing in
  let dotsfieldfn = donothing in
  let dotsenumdeclfn = donothing in
  let dotscasefn = donothing in
  let dotsdefparfn = donothing in
  let forinfofn = donothing in
  let casefn = donothing in
  let attributefn = donothing in
  let topfn = donothing in
  let enumdeclfn = donothing in

  (* --- These are shortened formatting functions that return MVSets --- *)

  (* Formats as <bef mn aft> where mn is extracted from meta_name mcode mc *)
  let meta_mc_format ~mc ~typ ~constr =
    mc_format ~rn ~mc ~totup_fn:(name_tup ~rn) ~before:typ ~after:constr in

  (* Formats as <typename str> where str is extracted from string mcode mc *)
  let str_mc_format ~mc ~typ =
    mc_format ~rn ~mc ~totup_fn:str_tup ~before:typ ~after:"" in

  (* Formats as <typename mn[listlen]>, mn extracted from meta_name mcode mc *)
  let lst_format ~mc ~typ ~listlen =
    list_format ~rn ~before:typ ~mc ~listlen in

  (* --- Implementations of functions that handle possible metavariables --- *)

  let identfn c fn v =
    match Ast0.unwrap v with
    | Ast0.MetaId(mc, constr, s, _) ->
        let constr = constraints ~rn constr in
        let seed = seed ~rn s in
        if seed = "" then (* if it has a seed then it is fresh ... ? *)
          meta_mc_format ~mc ~typ:"identifier " ~constr
        else
          meta_mc_format ~mc ~typ:"fresh identifier " ~constr:(constr ^ seed)
    | Ast0.MetaFunc(mc, constr, _) ->
        let constr = constraints ~rn constr in
        meta_mc_format ~mc ~typ:"function " ~constr
    | Ast0.MetaLocalFunc(mc, constr, _) ->
        let constr = constraints ~rn constr in
        meta_mc_format ~mc ~typ:"local function " ~constr
    | _ -> fn v in

  let stmtfn c fn v =
    match Ast0.unwrap v with
    | Ast0.MetaStmt (mc, constr, pure) ->
        let constr = constraints ~rn constr in
        meta_mc_format ~mc ~typ:"statement " ~constr
    | Ast0.MetaStmtList (mc, listlen, constr, _) ->
        lst_format ~mc ~typ:"statement list" ~listlen
    | Ast0.AsStmt (s1, s2)->
        let stmt = c.VT0.combiner_rec_statement in as_format s1 s2 stmt stmt
    | Ast0.Iterator (id, _, expdots, _, stmt,_) ->
        let expids = c.VT0.combiner_rec_expression_dots expdots in
        let stmtid = MVSet.union expids (c.VT0.combiner_rec_statement stmt) in
        let iteids = ids ~rn ~typ:"iterator" ~id in
        MVSet.union iteids stmtid
    | _ -> fn v in

  let exprfn c fn v =
    let exprfn = c.VT0.combiner_rec_expression in
    match Ast0.unwrap v with
    | Ast0.MetaErr (mc, constr, _) ->
        let constr = constraints ~rn constr in
        meta_mc_format ~mc ~typ:"error " ~constr
    | Ast0.MetaExpr (mc, constr, typeclist, form, _, _bitfield) ->
        let typeclist' =
          Common.map_option (List.map (Ast0toast.typeC false)) typeclist in

        (* types function finds metavariable types and identifiers that were
         * used in this expression and therefore need to be declared. *)
        let types = types ~rn typeclist' in

        (* type_c function returns the types in pretty string format *)
        let typ = type_c ~form typeclist' in
        let constr = constraints ~rn constr in
        MVSet.union (meta_mc_format ~mc ~typ ~constr) (types)
    | Ast0.MetaExprList (mc, listlen, _, _) ->
        lst_format ~mc ~typ:"expression list" ~listlen
    | Ast0.AsExpr (e1, e2) -> as_format e1 e2 exprfn exprfn
    | Ast0.AsSExpr (e1, s2) -> as_format e1 s2 exprfn stmtfn
    | _ -> fn v in

  let assignOpfn c fn v =
    match Ast0.unwrap v with
    | Ast0.MetaAssign (mc, constr, pure) ->
        let constr = constraints ~rn constr in
        meta_mc_format ~mc ~typ:"assignment operator " ~constr
    | _ -> fn v in

  let binaryOpfn c fn v =
    match Ast0.unwrap v with
    | Ast0.MetaBinary (mc, constr, pure) ->
        let constr  = constraints ~rn constr in
        meta_mc_format ~mc ~typ:"binary operator " ~constr
    | _ -> fn v in

  let tyfn c fn v =
    match Ast0.unwrap v with
    | Ast0.MetaType (mc, _, pure) -> meta_mc_format ~mc ~typ:"type " ~constr:""
    | Ast0.AsType (tc1, tc2) ->
        let ty = c.VT0.combiner_rec_typeC in as_format tc1 tc2 ty ty

    (* this clause generates unparsable scripts for who knows what reason ...
     * TODO: need to find out if it should be included or not. For now, ignore.
     *)
    | Ast0.TypeName mc ->
        let _ = str_mc_format ~mc ~typ:"typedef " in
        fn v
    | _ -> fn v in

  let initfn c fn v =
    match Ast0.unwrap v with
    | Ast0.MetaInit(mc, idconstr, pure) ->
        let constr = constraints ~rn idconstr in
        meta_mc_format ~mc ~typ:"initializer " ~constr
    | Ast0.MetaInitList(mc, listlen, _, pure) ->
        lst_format ~mc ~typ:"initializer list " ~listlen
    | Ast0.AsInit(i1,i2) ->
        let ini = c.VT0.combiner_rec_initialiser in as_format i1 i2 ini ini
    | _ -> fn v in

  let paramfn c fn v =
    match Ast0.unwrap v with
    | Ast0.MetaParam(mc, idconstr, pure) ->
        let constr = constraints ~rn idconstr in
        meta_mc_format ~mc ~typ:"parameter " ~constr
    | Ast0.MetaParamList(mc, listlen, _, pure) ->
        lst_format ~mc ~typ:"parameter list" ~listlen
    | Ast0.AsParam (ptd,ex) ->
        let par = c.VT0.combiner_rec_parameter in
        let expr = c.VT0.combiner_rec_expression in
        as_format ptd ex par expr
    | _ -> fn v in

(*
  Not used for now, visitor not parameterized by this...
  let define_paramfn c fn v =
    match Ast0.unwrap v with
    | Ast0.MetaDParamList(mc, listlen, pure) ->
        lst_format ~mc ~typ:"identifier list" ~listlen
    | _ -> fn v in
*)

  let declfn c fn v =
    match Ast0.unwrap v with
      Ast0.MetaDecl(mc, idconstr, pure) ->
        let constr = constraints ~rn idconstr in
        meta_mc_format ~mc ~typ:"declaration " ~constr
    | Ast0.AsDecl(dc1, dc2) ->
        let dec = c.VT0.combiner_rec_declaration in as_format dc1 dc2 dec dec
    | Ast0.MacroDecl(_, id, _, expdots, _, _, _) ->
        let expids = c.VT0.combiner_rec_expression_dots expdots in
        MVSet.union (ids ~rn ~typ:"declarer" ~id) expids
    | Ast0.MacroDeclInit(_, id, _, expdots, _, _, ini, _) ->
        let expids = c.VT0.combiner_rec_expression_dots expdots in
        let inid = MVSet.union expids (c.VT0.combiner_rec_initialiser ini) in
        let declids = ids ~rn ~typ:"declarer" ~id in
      MVSet.union declids inid
    | _ -> fn v in

  let fieldfn c fn v =
    match Ast0.unwrap v with
      Ast0.MetaField(mc, idconstr, pure) ->
        let constr = constraints ~rn idconstr in
	meta_mc_format ~mc ~typ:"field " ~constr
    | Ast0.MetaFieldList (mc, listlen, _, pure) ->
        lst_format ~mc ~typ:"field list" ~listlen
    | _ -> fn v in

  let string_fragmentfn c fn v =
    match Ast0.unwrap v with
    | Ast0.MetaFormatList(_, mc, _, listlen) ->
        lst_format ~mc ~typ:"format list" ~listlen
    | Ast0.FormatFragment(_, format) ->
      (match Ast0.unwrap format with
        | Ast0.MetaFormat(mc, idconstr) ->
            let constr = constraints rn idconstr in
            meta_mc_format ~mc ~typ:"format " ~constr
        | _ -> fn v
       )
    | _ -> fn v in

  let attr_argfn c fn v =
    match Ast0.unwrap v with
    | Ast0.MetaAttr(mc, idconstr, pure) ->
        let constr = constraints ~rn idconstr in
        meta_mc_format ~mc ~typ:"parameter " ~constr
    | _ -> fn v in

  V0.flat_combiner bind option_default
    meta_mcode string_mcode const_mcode simpleAssign_mcode opAssign_mcode
    fix_mcode unary_mcode arithOp_mcode logicalOp_mcode cv_mcode sign_mcode
    struct_mcode storage_mcode inc_mcode
    dotsexprfn dotsinitfn dotsparamfn dotsstmtfn dotsdeclfn dotsfieldfn
    dotsenumdeclfn dotscasefn dotsdefparfn
    identfn exprfn assignOpfn binaryOpfn tyfn initfn paramfn declfn fieldfn
    enumdeclfn stmtfn forinfofn casefn string_fragmentfn attributefn attr_argfn topfn


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

type t = meta_variable

let make ?(inherit_rule = "") ?(constraints = "") ~typ meta_name =
  make_mv typ (inherit_rule, meta_name) constraints

let get_rule (_,(r,_),_) = r
let get_name (_,(_,nm),_) = nm

(* forces rule inheritance (except if rule is already inherited). *)
let inherit_rule ?(force = false) ~new_rule ((a,(b,c),d) as mv) =
  if b = "" || force then (a,(new_rule,c),d) else mv

let print out mv = output_string out (tostring_mv mv)

(* prints the strings in the set on separate lines, ended with semicolons.
 * if do_group, group all metavars of same type on the same line.
 *)
let print_list out ~do_group mvs =
  let group_by_type mvs =
    let rec group acc = function
      | [] -> acc
      | ((t,_,_)::ls as b) ->
          let (same, rest) = List.partition (fun (x,_,_) -> t = x) b in
          group ((t, same) :: acc) rest in
    let collapse_group (typ, lst) =
      let mvs = List.map (fun (_,b,c) -> tostring_mv ("",b,c)) lst in
      typ ^ (String.concat ", " mvs) in
    List.rev_map collapse_group (group [] mvs) in

  if do_group then begin
    let grouped = group_by_type mvs in
    List.iter (fun b -> output_string out (b ^ ";\n")) grouped
  end else
    List.iter (fun b -> output_string out ((tostring_mv b) ^ ";\n")) mvs

(* takes abstract syntax trees for a rule and extract all metavariables.
 * That is, metavariables declared in the header, but unused in the body, are
 * discarded. Returns list of meta_variable.t's.
 *)
let extract ~minus_rule ~rule_name =
  let mvcomb = metavar_combiner rule_name in
  let minus = List.map mvcomb.VT0.combiner_rec_top_level minus_rule in
  let comb = List.fold_left MVSet.union MVSet.empty minus in
  MVSet.elements comb
