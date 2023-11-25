module Ast = Ast_cocci
module V = Visitor_ast

let ocaml_support = false

exception CompileFailure of string
exception LinkFailure of string * string

let do_toplevel =
  let bind a b = a || b in
  let check_constraint = function
      Ast_cocci.CstrScript(_,(_,"ocaml",_,_,_)) -> true
    | _ -> false in
  let check_constraints csts = List.exists check_constraint csts in
  let bind_wrap k a b =
    bind (check_constraints(List.map snd (Ast.get_constraints a)))
      (bind (k a) b) in
  let option_default = false in
  let donothing recursor k e = k e in (* just combine in the normal way *)
  let donothing_a recursor k e = k e in (* anything is not wrapped *)
  let mcodefn r mc =
    List.fold_left bind option_default
      (List.map
	 (function
	     Ast.MetaPos(name,constraints,_,_,_)
	   | Ast.MetaCom(name,constraints,_,_) ->
	       check_constraint constraints)
	 (Ast.get_pos_var mc)) in

  let identfn r k i =
    bind_wrap k i
      (match Ast.unwrap i with
	Ast.MetaId(name,idconstraint,_,_) | Ast.MetaFunc(name,idconstraint,_,_)
      | Ast.MetaLocalFunc(name,idconstraint,_,_) -> check_constraint idconstraint
      | _ -> false) in

  let listlen = function
      Ast.MetaListLen(lenname,cstr,_,_) -> check_constraint cstr
    | _ -> option_default in

  let listlen_option l =
    match l with
      Some l -> listlen l
    | _ -> option_default in

  let exprfn r k e =
    bind_wrap k e
      (match Ast.unwrap e with
	Ast.MetaErr(name,cstr,_,_) -> check_constraint cstr
      | Ast.MetaExpr(name,cstr,_,_,_,_,bitfield) ->
	  bind (check_constraint cstr) (listlen_option bitfield)
      | Ast.MetaExprList(name,len,cstr,_,_) ->
	  bind (check_constraint cstr) (listlen len)
      | _ -> false) in

  let fragfn r k ft =
    bind_wrap k ft
      (match Ast.unwrap ft with
	Ast.MetaFormatList
	  (pct,name,len,cstr,_,_) ->
	    bind (check_constraint cstr) (listlen len)
      | _ -> option_default) in

  let fmtfn r k ft =
    bind_wrap k ft
      (match Ast.unwrap ft with
	Ast.MetaFormat(name,cstr,_,_) -> check_constraint cstr
      | _ -> option_default) in

  let assignOpfn r k aop =
    bind_wrap k aop
      (match Ast.unwrap aop with
	Ast.MetaAssign(name,cstr,_,_) -> check_constraint cstr
      | _ -> option_default) in
  let binaryOpfn r k bop =
    bind_wrap k bop
      (match Ast.unwrap bop with
	Ast.MetaBinary(name,cstr,_,_) -> check_constraint cstr
      | _ -> option_default) in
  let pragmainfofn r k pm =
    bind_wrap k pm
      (match Ast.unwrap pm with
	Ast.MetaPragmaInfo(name,cstr,_,_) -> check_constraint cstr
      | _ -> option_default) in

  let tyfn r k ty =
    bind_wrap k ty
      (match Ast.unwrap ty with
	Ast.MetaType(name,cstr,_,_) -> check_constraint cstr
      | _ -> option_default) in
  let initfn r k i =
    bind_wrap k i
      (match Ast.unwrap i with
	Ast.MetaInit(name,cstr,_,_) -> check_constraint cstr
      | Ast.MetaInitList(name,len,cstr,_,_) ->
	  bind (check_constraint cstr) (listlen len)
      | _ -> option_default) in
  let paramfn r k p =
    bind_wrap k p
      (match Ast.unwrap p with
	Ast.MetaParam(name,cstr,_,_) -> check_constraint cstr
      | Ast.MetaParamList(name,len,cstr,_,_) ->
	  bind (check_constraint cstr) (listlen len)
      | _ -> option_default) in
  let define_paramfn r k p =
    bind_wrap k p
      (match Ast.unwrap p with
	Ast.MetaDParamList(name,len,cstr,_,_) ->
	  bind (check_constraint cstr) (listlen len)
      | _ -> option_default) in
  let declfn r k d =
    bind_wrap k d
      (match Ast.unwrap d with
	Ast.MetaDecl(name,cstr,_,_) -> check_constraint cstr
      | _ -> option_default) in

  let fieldfn r k d =
    bind_wrap k d
      (match Ast.unwrap d with
	Ast.MetaField(name,cstr,_,_) -> check_constraint cstr
      | Ast.MetaFieldList(name,len,cstr,_,_) ->
	  bind (check_constraint cstr) (listlen len)
      | _ -> option_default) in
  let rulefn r k re =
    bind_wrap k re
      (match Ast.unwrap re with
	Ast.MetaRuleElem(name,cstr,_,_) | Ast.MetaStmt(name,cstr,_,_,_) ->
	  check_constraint cstr
      | Ast.MetaStmtList(name,len,cstr,_,_) ->
	  bind (check_constraint cstr) (listlen len)
      | _ -> option_default) in

  let attr_argfn r k a =
    bind_wrap k a
      (match Ast.unwrap a with
	Ast.MetaAttr(name,cstr,_,_) -> check_constraint cstr
      | _ -> option_default) in

  (V.combiner bind option_default
     mcodefn mcodefn mcodefn mcodefn mcodefn mcodefn
     mcodefn mcodefn mcodefn
     mcodefn mcodefn mcodefn mcodefn mcodefn
     donothing donothing donothing donothing donothing donothing donothing
     identfn exprfn fragfn fmtfn assignOpfn binaryOpfn pragmainfofn
     donothing tyfn initfn paramfn define_paramfn declfn
     donothing fieldfn donothing donothing rulefn donothing
     donothing donothing attr_argfn donothing donothing_a).V.combiner_top_level

let prepare coccifile code =
  let ocamls_rules =
    List.fold_left
      (function prev ->
	function
	    Ast_cocci.ScriptRule (name,"ocaml",deps,mv,script_vars,_pos,code) ->
	      true
	  | Ast_cocci.InitialScriptRule (name,"ocaml",deps,mvs,_pos,code) ->
	      true
	  | Ast_cocci.FinalScriptRule (name,"ocaml",deps,mvs,_pos,code) ->
	      true
	  | Ast_cocci.CocciRule (nm, rule_info, r, is_exp,ruletype) ->
	      (* a case for every metavariable that can have a constraint *)
	      prev ||
	      (List.fold_left (fun prev cur -> prev || do_toplevel cur)
		false r)
	  | _ -> prev)
      false code in
  if ocamls_rules
  then failwith "OCaml scripting is unsupported."
  else None

let prepare_simple _ =
  failwith "OCaml scripting is unsupported. Compile spatch with OCaml version >= 3.11"

let load_file mlfile = ()
let clean_file mlfile = ()
let test () = ()
