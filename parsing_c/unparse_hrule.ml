(*
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2006, 2007 Julia Lawall
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *
 * This file was part of Coccinelle.
 *)

module Ast = Ast_cocci
module V = Visitor_ast

let error x s =
  failwith
    (Printf.sprintf "unparse_hrule: line: %d, %s" (Ast.get_line x) s)

let names = ref ([] : (string * int ref) list)

let started_files = ref ([] : (string * bool) list)
let typedefs = ref ([] : (string * string list ref) list)
let current_outfile = ref ""

let prefix = "_cocci_"

(* ----------------------------------------------------------------------- *)
(* Create rule to check for header include *)

let print_header_rule pr srcfile =
  match Str.split (Str.regexp "/") srcfile with
    [x] ->
      pr "@header@\n@@\n\n#include \"";
      pr x; pr "\"\n\n"; true
  | l ->
      let rec loop = function
	  [] -> false
	| [x] ->
	    pr "@header@\n@@\n\n#include \"";
	    pr x; pr "\"\n\n"; true
	| "include"::(x::xs) ->
	    pr "@header@\n@@\n\n#include <";
	    let x =
	      if Str.string_match (Str.regexp "asm-") x 0 then "asm" else x in
	    pr (String.concat "/" (x::xs));
	    pr ">\n\n"; true
	| x::xs -> loop xs in
      loop l

(* ----------------------------------------------------------------------- *)
(* Print check that we are not in the defining function *)

let print_check_rule pr function_name function_name_count header_req =
  (if header_req
  then pr (Printf.sprintf "@same_%s depends on header@\n" function_name_count)
  else pr (Printf.sprintf "@same_%s@\n" function_name_count));
  pr "position p;\n";
  pr "@@\n\n";
  pr function_name; pr "@p(...) { ... }\n\n"

(* ----------------------------------------------------------------------- *)
(* get parameters of the matched function *)

let rec env_lookup fn = function
    [] -> failwith "no binding"
  | (nm,vl)::rest when fn nm -> vl
  | _::rest -> env_lookup fn rest

let get_paramst env =
  let argname = ref ("","") in
  let fn ((_,nm) as name) =
    if nm = "ARGS"
    then (argname := name; true)
    else false in
  match env_lookup fn env with
    Ast_c.MetaParamListVal(paramst) -> (paramst,!argname)
  | _ -> failwith "not possible"

let get_function_name rule env =
  let donothing r k e = k e in
  let option_default = [] in
  let bind = Common.union_set in
  let do_any_list_list r any_list_list =
    List.fold_left
      (List.fold_left
	 (function prev -> function cur ->
	   bind (r.V.combiner_anything cur) prev))
      [] any_list_list in
  let mcode r mc =
    match Ast.get_mcodekind mc with
      Ast.MINUS(_,_,_,any_list_list) ->
	(match any_list_list with
	  Ast.NOREPLACEMENT -> []
	| Ast.REPLACEMENT(any_list_list,_) ->
	    do_any_list_list r any_list_list)
    | Ast.CONTEXT(_,any_befaft) ->
	(match any_befaft with
	  Ast.BEFORE(any_list_list,_) | Ast.AFTER(any_list_list,_) ->
	    do_any_list_list r any_list_list
	| Ast.BEFOREAFTER(ba,aa,_) ->
	    bind (do_any_list_list r ba) (do_any_list_list r aa)
	| Ast.NOTHING -> [])
    | Ast.PLUS _ -> [] in
  let expression r k e =
    bind (k e)
    (match Ast.unwrap e with
      Ast.FunCall(fn,lp,args,rp) ->
	(match Ast.unwrap args with
	  [e] ->
	    (match Ast.unwrap e with
	      Ast.MetaExprList(nm,_,_,_,_) ->
		(match (Ast.unwrap_mcode nm,Ast.get_mcodekind nm) with
		  ((_,"ARGS"), Ast.PLUS _) ->
		    (match Ast.unwrap fn with
		      Ast.Ident(id) ->
			(match Ast.unwrap id with
			  Ast.MetaId(nm,_,_,_)
			| Ast.MetaFunc(nm,_,_,_)
			| Ast.MetaLocalFunc(nm,_,_,_) ->
			    [Ast.unwrap_mcode nm]
			| _ -> [])
		    | _ -> [])
		| _ -> [])
	    | _ -> [])
	| _ -> [])
    | _ -> []) in
  let names =
    (V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing expression donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing
      donothing donothing donothing donothing donothing donothing).V.combiner_top_level
      rule in
  match names with
    [name] ->
      (match env_lookup (function nm -> nm = name) env with
	Ast_c.MetaIdVal(s) | Ast_c.MetaFuncVal(s)
      | Ast_c.MetaLocalFuncVal(s) -> s
      |	_ -> error rule "not possible")
  | _ -> error rule "inconsistent rule generation"

(* ----------------------------------------------------------------------- *)
(* Print metavariable declarations *)

let rec print_typedef pr = function
    (Ast_c.TypeName(name,_),_) ->
      let s = Ast_c.str_of_name name in
      let typedefs =
	try List.assoc !current_outfile !typedefs
	with Not_found ->
	  let td = ref [] in
	  typedefs := (!current_outfile,td)::!typedefs;
	  td in
      if not (List.mem s !typedefs)
      then (typedefs := s::!typedefs; pr "typedef "; pr s; pr ";\n")
  | (Ast_c.Pointer(_,ty),_) -> print_typedef pr ty
  | _ -> ()

let rewrap_str s ii =
  {ii with Ast_c.pinfo =
    (match ii.Ast_c.pinfo with
      Ast_c.OriginTok pi ->
	Ast_c.OriginTok { pi with Common.str = s;}
    | Ast_c.ExpandedTok (pi,vpi) ->
	Ast_c.ExpandedTok ({ pi with Common.str = s;},vpi)
    | Ast_c.FakeTok (_,vpi) -> Ast_c.FakeTok (s,vpi)
    | Ast_c.AbstractLineTok pi ->
	Ast_c.AbstractLineTok { pi with Common.str = s;})}

let rewrap_prefix_name prefix name =
  match name with
  | Ast_c.RegularName (s, iiname) ->
      let iis = Common.tuple_of_list1 iiname in
      let iis' = rewrap_str (prefix^s) iis in
      Ast_c.RegularName (prefix ^ s, [iis'])
  | Ast_c.CppConcatenatedName _ | Ast_c.CppVariadicName _
  | Ast_c.CppIdentBuilder _
      -> raise Common.Todo


let print_metavar pr = function
  | {Ast_c.p_namei = Some name;
     p_type = (_,(Ast_c.Pointer(_,(Ast_c.BaseType(Ast_c.Void),_)),_));
    }
    ->
      let param = Ast_c.str_of_name name in
      pr ("expression "^prefix); pr param
  | ({Ast_c.p_namei = Some name; p_type = (_,ty)} : Ast_c.parameterType) ->

      let name' = rewrap_prefix_name prefix name in

      print_typedef pr ty;

      Pretty_print_c.pp_param_gen
	(function x ->
	  let str = Ast_c.str_of_info x in
	  if not (List.mem str ["const";"volatile"])
	  then pr str)
	(function _ -> pr " ")
        {Ast_c.p_register = (false,[]);
         p_namei = Some name';
         p_type = (({Ast_c.const = false; Ast_c.volatile = false},[]),ty);
         p_attr = [];
        }
  | _ -> failwith "function must have named parameters"

let make_exp = function
    ({Ast_c.p_namei = Some name; p_type = ty}, comma_ii) ->
      let no_info = (None,Ast_c.NotTest) in

      let name' = rewrap_prefix_name prefix name in

      let exp =
	((Ast_c.Ident (name'),ref no_info),Ast_c.noii) in
      (name,(Common.Left exp,comma_ii))
  | _ -> failwith "bad parameter"

let print_extra_typedefs pr env =
  let bigf =
    { Visitor_c.default_visitor_c with
      Visitor_c.ktype = (fun (k, bigf) ty ->
	match ty with
	  (_,((Ast_c.TypeName(_,_),_) as ty)) -> print_typedef pr ty
	| _ -> k ty) } in
  List.iter
    (function (_,vl) ->
      match vl with
	Ast_c.MetaIdVal(_) | Ast_c.MetaFuncVal(_)
      | Ast_c.MetaAssignOpVal(_) | Ast_c.MetaBinaryOpVal(_)
      | Ast_c.MetaLocalFuncVal(_) -> ()
      | Ast_c.MetaExprVal(exp,_,_) -> Visitor_c.vk_expr bigf exp
      | Ast_c.MetaExprListVal(args) -> Visitor_c.vk_argument_list bigf args
      | Ast_c.MetaParamVal(param) -> Visitor_c.vk_param bigf param
      | Ast_c.MetaParamListVal(params) -> Visitor_c.vk_param_list bigf params
      | Ast_c.MetaDParamListVal(params) ->
	  Visitor_c.vk_define_params bigf params

      | Ast_c.MetaTypeVal(ty) -> Visitor_c.vk_type bigf ty
      | Ast_c.MetaInitVal(ty) -> Visitor_c.vk_ini bigf ty
      | Ast_c.MetaInitListVal(ty) -> Visitor_c.vk_ini_list bigf ty
      | Ast_c.MetaDeclVal(decl,_) -> Visitor_c.vk_decl bigf decl
      | Ast_c.MetaFieldVal(field) -> Visitor_c.vk_struct_field bigf field
      | Ast_c.MetaFieldListVal(fields) ->
	  Visitor_c.vk_struct_fields bigf fields
      | Ast_c.MetaFmtVal(fmt) -> Visitor_c.vk_string_format bigf fmt
      | Ast_c.MetaAttrArgVal(name) -> Visitor_c.vk_attr_arg bigf name
      | Ast_c.MetaFragListVal(frags) ->
	  Visitor_c.vk_string_fragments bigf frags
      | Ast_c.MetaStmtVal(stm,_,_) -> Visitor_c.vk_statement bigf stm
      | Ast_c.MetaStmtListVal(stms,_) ->
	  Visitor_c.vk_statement_sequencable_list bigf stms
      | Ast_c.MetaPosVal _ | Ast_c.MetaPosValList _ | Ast_c.MetaComValList _
      | Ast_c.MetaListlenVal _ -> ()
      | Ast_c.MetaNoVal -> failwith "referencing a metavar with no value")
    env

let rename argids env =
  let argenv = List.map (function name ->
    let arg = Ast_c.str_of_name name in
    (arg,prefix^arg)
  ) argids in
  let lookup x = try List.assoc x argenv with Not_found -> x in
  let bigf =
    { Visitor_c.default_visitor_c_s with
    Visitor_c.kexpr_s = (fun (k,bigf) e ->
      match e with
	((Ast_c.Ident (name), info), []) ->

          (* pad: assert is_regular_ident ? *)
          let s = Ast_c.str_of_name name in
          let ii = Ast_c.info_of_name name in
	  let new_name = lookup s in
          let new_id = Ast_c.RegularName (new_name, [rewrap_str new_name ii]) in
	  ((Ast_c.Ident (new_id), info), Ast_c.noii)
      | _ -> k e) } in
  List.map
    (function (x,vl) ->
      (x,
       match vl with
	 Ast_c.MetaIdVal(_) | Ast_c.MetaFuncVal(_)
       | Ast_c.MetaAssignOpVal(_) | Ast_c.MetaBinaryOpVal(_)
       | Ast_c.MetaLocalFuncVal(_) -> vl
       | Ast_c.MetaExprVal(exp,c,ty) ->
	   Ast_c.MetaExprVal(Visitor_c.vk_expr_s bigf exp,c,ty)
       | Ast_c.MetaExprListVal(args) ->
	   Ast_c.MetaExprListVal(Visitor_c.vk_arguments_s bigf args)
       | Ast_c.MetaParamVal(param) ->
	   Ast_c.MetaParamVal(Visitor_c.vk_param_s bigf param)
       | Ast_c.MetaParamListVal(params) ->
	   Ast_c.MetaParamListVal(Visitor_c.vk_params_s bigf params)
       | Ast_c.MetaDParamListVal(params) ->
	   Ast_c.MetaDParamListVal(Visitor_c.vk_define_params_s bigf params)

       | Ast_c.MetaTypeVal(ty) ->
	   Ast_c.MetaTypeVal(Visitor_c.vk_type_s bigf ty)
       | Ast_c.MetaInitVal(ini) ->
	   Ast_c.MetaInitVal(Visitor_c.vk_ini_s bigf ini)
       | Ast_c.MetaInitListVal(ini) ->
	   Ast_c.MetaInitListVal(Visitor_c.vk_inis_s bigf ini)
       | Ast_c.MetaDeclVal(stm,original) ->
	   Ast_c.MetaDeclVal(Visitor_c.vk_decl_s bigf stm,
			     Visitor_c.vk_decl_s bigf original)
       | Ast_c.MetaFieldVal(stm) ->
	   Ast_c.MetaFieldVal(Visitor_c.vk_struct_field_s bigf stm)
       | Ast_c.MetaFieldListVal(stm) ->
	   Ast_c.MetaFieldListVal(Visitor_c.vk_struct_fields_s bigf stm)
       | Ast_c.MetaFmtVal(fmt) ->
	   Ast_c.MetaFmtVal(Visitor_c.vk_string_format_s bigf fmt)
       | Ast_c.MetaAttrArgVal(name) ->
	   Ast_c.MetaAttrArgVal(Visitor_c.vk_attr_arg_s bigf name)
       | Ast_c.MetaFragListVal(frags) ->
	   Ast_c.MetaFragListVal(Visitor_c.vk_string_fragments_s bigf frags)
       | Ast_c.MetaStmtVal(stm,original,ty) ->
	   Ast_c.MetaStmtVal(Visitor_c.vk_statement_s bigf stm,
			     Visitor_c.vk_statement_s bigf original,ty)
       | Ast_c.MetaStmtListVal(stm,ty) ->
	   Ast_c.MetaStmtListVal
	     (Visitor_c.vk_statement_sequencable_list_s bigf stm,ty)
       | Ast_c.MetaPosVal _ | Ast_c.MetaPosValList _ | Ast_c.MetaComValList _
       | Ast_c.MetaListlenVal _ -> vl
       | Ast_c.MetaNoVal -> failwith "referencing a metavar with no value"))
    env

let print_one_type pr env ty =
  match Common.map_option Ast.unwrap (Ast.typeC_of_fullType_opt ty) with
    Some (Ast_cocci.MetaType(name,cstr,keep,inherited)) ->
      (try
	match List.assoc name env with
	  Ast_c.MetaTypeVal ty ->
	    Pretty_print_c.pp_type_gen
	      (function x -> pr (Ast_c.str_of_info x))
	      (function _ -> pr " ")
	      ty
        | _ -> failwith "impossible"
      with Not_found -> pr (Ast_cocci.string_of_fullType ty))
  | _ -> pr (Ast_cocci.string_of_fullType ty)

let print_types pr env = function
    None -> ()
  | Some [ty] -> print_one_type pr env ty
  | Some types ->
      pr "{";
      Common.print_between (function _ -> pr ", ") (print_one_type pr env)
	types;
      pr "}"

let pp_len pr len =
  let pp_name (_,n) = pr n in
  match len with
    Ast.AnyLen -> ()
  | Ast.MetaLen (len,_) -> pr "["; pp_name len; pr "]"
  | Ast.CstLen len -> pr "["; pr (string_of_int len); pr "]"

let pp_meta_decl pr env decl =
  let no_arity = function Ast.NONE -> () | _ -> failwith "no arity allowed" in
  let pp_name (_,n) = pr n in
  match decl with
    Ast.MetaMetaDecl(ar, name) ->
      (* ignore virtual *)
      no_arity ar; pr "metavariable "; pp_name name; pr ";\n"
  | Ast.MetaIdDecl(ar, name) ->
      (* ignore virtual *)
      no_arity ar; pr "identifier "; pp_name name; pr ";\n"
  | Ast.MetaFreshIdDecl(name, Ast.NoVal) ->
      pr "fresh identifier "; pp_name name; pr ";\n"
  | Ast.MetaFreshIdDecl(name, Ast.StringSeed x) ->
      pr "fresh identifier "; pp_name name; pr " = \""; pr x; pr "\";\n"
  | Ast.MetaFreshIdDecl(name, Ast.ListSeed x) ->
      failwith "unparse_hrule: not supported"
  | Ast.MetaFreshIdDecl(name, Ast.ScriptSeed x) ->
      failwith "unparse_hrule: not supported"
  | Ast.MetaTypeDecl(ar, name) ->
      no_arity ar; pr "type "; pp_name name; pr ";\n"
  | Ast.MetaInitDecl(ar, name) ->
      no_arity ar; pr "initialiser "; pp_name name; pr ";\n"
  | Ast.MetaInitListDecl(ar, name, len) ->
      no_arity ar; pr "initialiser list "; pp_len pr len; pp_name name;
      pr ";\n"
  | Ast.MetaListlenDecl(name) -> ()
  | Ast.MetaParamDecl(ar, name) ->
      no_arity ar; pr "parameter "; pp_name name; pr ";\n"
  | Ast.MetaParamListDecl(ar, name, len) ->
      no_arity ar; pr "parameter list "; pp_len pr len; pp_name name; pr ";\n"
  | Ast.MetaDParamListDecl(ar, name, len) ->
      no_arity ar; pr "identifier list "; pp_len pr len; pp_name name; pr ";\n"
  | Ast.MetaBinaryOperatorDecl(ar, name) ->
      no_arity ar; pr "binary operator "; pp_name name; pr ";\n"
  | Ast.MetaAssignmentOperatorDecl(ar, name) ->
      no_arity ar; pr "assignment operator "; pp_name name; pr ";\n"
  | Ast.MetaConstDecl(ar, name, types) ->
      no_arity ar; pr "constant "; print_types pr env types;
      pp_name name; pr ";\n"
  | Ast.MetaErrDecl(ar, name) ->
      no_arity ar; pr "error "; pp_name name; pr ";\n"
  | Ast.MetaExpDecl(ar, name, None, _bitfield) ->
      no_arity ar; pr "expression "; pp_name name; pr ";\n"
  | Ast.MetaExpDecl(ar, name, types, _bitfield) ->
      no_arity ar; print_types pr env types; pp_name name; pr ";\n"
  | Ast.MetaIdExpDecl(ar, name, types) ->
      no_arity ar; pr "idexpression ";
      print_types pr env types; pp_name name; pr ";\n"
  | Ast.MetaLocalIdExpDecl(ar, name, types) ->
      no_arity ar; pr "local idexpression ";
      print_types pr env types; pp_name name; pr ";\n"
  | Ast.MetaGlobalIdExpDecl(ar, name, types) ->
      no_arity ar; pr "global idexpression ";
      print_types pr env types; pp_name name; pr ";\n"
  | Ast.MetaExpListDecl(ar, name, len) ->
      no_arity ar; pr "parameter list "; pp_len pr len; pp_name name; pr ";\n"
  | Ast.MetaDeclDecl(ar, name) ->
      no_arity ar; pr "declaration "; pp_name name; pr ";\n"
  | Ast.MetaFieldDecl(ar, name) ->
      no_arity ar; pr "field "; pp_name name; pr ";\n"
  | Ast.MetaFieldListDecl(ar, name, len) ->
      no_arity ar; pr "field list "; pp_len pr len; pp_name name; pr ";\n"
  | Ast.MetaStmDecl(ar, name) ->
      no_arity ar; pr "statement "; pp_name name; pr ";\n"
  | Ast.MetaStmListDecl(ar, name, len) ->
      no_arity ar; pr "statement list "; pp_len pr len; pp_name name; pr ";\n"
  | Ast.MetaFuncDecl(ar, name) ->
      no_arity ar; pr "function "; pp_name name; pr ";\n"
  | Ast.MetaLocalFuncDecl(ar, name) ->
      no_arity ar; pr "local function "; pp_name name; pr ";\n"
  | Ast.MetaPosDecl(ar, name) ->
      no_arity ar; pr "position "; pp_name name; pr ";\n"
  | Ast.MetaComDecl(ar, name) ->
      no_arity ar; pr "comments "; pp_name name; pr ";\n"
  | Ast.MetaFmtDecl(ar, name) ->
      no_arity ar; pr "format "; pp_name name; pr ";\n"
  | Ast.MetaAttributeDecl(ar, name) ->
      no_arity ar; pr "attribute "; pp_name name; pr ";\n"
  | Ast.MetaFragListDecl(ar, name, len) ->
      no_arity ar; pr "format list "; pp_len pr len; pp_name name; pr ";\n"
  | Ast.MetaAnalysisDecl(code, name) ->
      pr "analysis"; pr code; pr " "; pp_name name; pr ";\n"
  | Ast.MetaDeclarerDecl(ar, name) ->
      no_arity ar; pr "declarer "; pp_name name; pr ";\n"
  | Ast.MetaIteratorDecl(ar, name) ->
      no_arity ar; pr "iterator "; pp_name name; pr ";\n"
  | Ast.MetaScriptDecl(_, name) -> failwith "script metavariable"

let print_metavariables pr local_metas paramst env header_req function_name =
  (if header_req
  then pr "@depends on header@\n"
  else pr "@@\n");
  pr (Printf.sprintf "position _p!=same_%s.p;\n" function_name);
  pr "identifier _f;\n";
  let rec loop = function
      [] | [{Ast_c.p_type =(_,(Ast_c.BaseType(Ast_c.Void),_))},_] -> []
    | ((first,_) as f)::rest ->
	print_metavar pr first; pr ";\n";
	(make_exp f) :: loop rest in
  let args = loop paramst in
  print_extra_typedefs pr env;
  List.iter (pp_meta_decl pr env) local_metas;
  pr "@@\n\n";
  args

(* ----------------------------------------------------------------------- *)
(* print_start/end *)

let print_start pr =
  pr "_f@_p(...) { <+...\n"

let print_end pr =
  pr "\n...+> }\n"

(* ----------------------------------------------------------------------- *)
(* Print call to the defined function *)

let print_param_name pr = function
    {Ast_c.p_namei = Some name} -> pr (Ast_c.str_of_name name)
  | _ -> failwith "function must have named parameters"

let pp_def_gen pr defn isexp =
  let {Ast_c.f_name = name; f_type = (_, (paramst, (b, iib))); } = defn in
  pr (Ast_c.str_of_name name); pr "(";
  (if b then failwith "not handling variable argument functions");
  (match paramst with
    [] | [{Ast_c.p_type = (_,(Ast_c.BaseType(Ast_c.Void),_))},_] -> ()
  | (first,_)::rest ->
      print_param_name pr first;
      List.iter (function (x,_) -> pr ", "; print_param_name pr x) rest);
  pr ")"; if not isexp then pr ";"

(* ----------------------------------------------------------------------- *)
(* Entry point *)

let pp_rule local_metas ast env srcfile =
  let (paramst,args_name) = get_paramst env in
  (* get rule information *)
  let (rule,printable) =
    match ast with
      Ast.CocciRule(_,_,[body],_,_) -> (* could extend to use attributes *)
	(body,
	 match Ast.unwrap body with
	   Ast.NONDECL(s) -> [[Ast.StatementTag s]]
	 | Ast.CODE(ss) -> [[Ast.StmtDotsTag ss]]
	 | _ -> error body "bad rule body")
    | _ -> failwith "bad rule" in
  (* create the output file *)
  let outdir =
    match !Flag.make_hrule with
      Some outdir -> outdir
    | None -> error rule "not possible" in
  let function_name = get_function_name rule env in
  let function_name_count =
    try
      let cell = List.assoc function_name !names in
      let ct = !cell in
      cell := ct + 1;
      function_name ^ (string_of_int ct)
    with Not_found ->
      let cell = ref 1 in
      names := (function_name,cell) :: !names;
      function_name in
  let outfile = outdir ^ "/" ^
    (if !Flag.hrule_per_file
    then Filename.chop_extension (Filename.basename srcfile)
    else function_name_count) in
  let escape_re = Str.regexp_string "/" in
  let dir = if !Flag.dir = "" then Filename.dirname srcfile else !Flag.dir in
  let outdirfile = Str.global_replace escape_re "_"dir in
  let outfile = outfile ^ outdirfile ^ ".cocci" in
  let saved_header_req =
    try let res = List.assoc outfile !started_files in Some res
    with Not_found -> None in
  current_outfile := outfile;
  Common.with_open_outfile_append outfile (fun (pr,chan) ->
    let header_req =
      match saved_header_req with
	Some x -> x
      |	None ->
	  let res = print_header_rule pr srcfile in
	  started_files := (outfile,res)::!started_files;
	  res in
    print_check_rule pr function_name function_name_count header_req;
    let env' = List.map (fun (a, b) -> (Ast_cocci.make_mcode a, b)) env in
    let args =
      print_metavariables pr local_metas paramst env' header_req
	function_name_count in
    let (argids,args) = List.split args in
    let env = rename argids env in
    let env = (args_name,Ast_c.MetaExprListVal args)::env in
    print_start pr;
    (* for printing C tokens *)
    let pr_c info =
      match Ast_c.pinfo_of_info info with
	Ast_c.AbstractLineTok _ -> pr (Ast_c.str_of_info info)
      | Ast_c.FakeTok (s,_) -> pr s
      |	_ ->
	  Printf.printf "line: %s\n" (Dumper.dump info);
	  error rule "not an abstract line" in
    let pr_space _ = pr " " in
    Unparse_cocci.pp_list_list_any
      ([env], (fun s _ _ _ _ -> pr s), pr_c, pr_space, pr_space, pr,
       (fun _ _ -> ()), (function _ -> ()), (function _ -> ()),
       (function _ -> ()))
      true printable Unparse_cocci.InPlace;
    print_end pr;
    pr "\n")
