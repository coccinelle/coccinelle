module Ast = Ast_cocci
module V = Visitor_ast

let error x s =
  failwith
    (Printf.sprintf "unparse_hrule: line: %d, %s" (Ast.get_line x) s)

let names = ref ([] : (string * int ref) list)

(* ----------------------------------------------------------------------- *)
(* Create rule to check for header include *)

let print_header_rule pr srcfile =
  match Str.split (Str.regexp "/") srcfile with
    [x] ->
      pr "@header@\n@@\n\n#include \""; pr x; pr "\"\n\n"; true
  | l ->
      let rec loop = function
	  [] -> false
	| [x] ->
	    pr "@header@\n@@\n\n#include \""; pr x; pr "\"\n\n"; true
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

let print_check_rule pr function_name header_req =
  (if header_req
  then pr "@same depends on header@\n"
  else pr "@same@\n");
  pr "position p;\n";
  pr "@@\n\n";
  pr function_name; pr "@p(...) { ... }\n\n"

(* ----------------------------------------------------------------------- *)
(* get paramaters of the matched function *)

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
      Ast.MINUS(_,any_list_list) -> do_any_list_list r any_list_list
    | Ast.CONTEXT(_,any_befaft) ->
	(match any_befaft with
	  Ast.BEFORE(any_list_list) | Ast.AFTER(any_list_list) ->
	    do_any_list_list r any_list_list
	| Ast.BEFOREAFTER(ba,aa) ->
	    bind (do_any_list_list r ba) (do_any_list_list r aa)
	| Ast.NOTHING -> [])
    | Ast.PLUS -> [] in
  let expression r k e =
    bind (k e)
    (match Ast.unwrap e with
      Ast.FunCall(fn,lp,args,rp) ->
	(match Ast.undots args with
	  [e] ->
	    (match Ast.unwrap e with
	      Ast.MetaExprList(nm,_,_,_) ->
		(match Ast.unwrap_mcode nm with
		  (_,"ARGS") when Ast.get_mcodekind nm = Ast.PLUS ->
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
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode

      donothing donothing donothing donothing
      donothing expression donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing).V.combiner_top_level
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

let rec print_typedef pr typedefs = function
    (Ast_c.TypeName(s,_),_) ->
      if not (List.mem s !typedefs)
      then (typedefs := s::!typedefs; pr "typedef "; pr s; pr ";\n")
  | (Ast_c.Pointer(_,ty),_) -> print_typedef pr typedefs ty
  | _ -> ()

let print_metavar pr typedefs = function
    ((_,Some param,(_,(Ast_c.Pointer(_,(Ast_c.BaseType(Ast_c.Void),_)),_))),_)
    ->
      pr "expression "; pr param
  | (((_,Some param,(_,ty)),il) : Ast_c.parameterType) ->
      print_typedef pr typedefs ty;
      Pretty_print_c.pp_param_gen
	(function x ->
	  let str = Ast_c.str_of_info x in
	  if not (List.mem str ["const";"volatile"])
	  then pr str)
	(function _ -> pr " ")
	((false,Some param,
	  (({Ast_c.const = false; Ast_c.volatile = false},[]),ty)),
	 il)
  | _ -> failwith "function must have named parameters"

let make_exp = function
    (((_,Some name,ty),param_ii),comma_ii) ->
      let no_info = (None,Ast_c.NotTest) in
      let exp =
	((Ast_c.Ident name,ref no_info),[List.hd(List.rev param_ii)]) in
      (Common.Left exp,comma_ii)
  | _ -> failwith "bad parameter"

let print_extra_typedefs pr typedefs env =
  let bigf =
    { Visitor_c.default_visitor_c with
      Visitor_c.ktype = (fun (k, bigf) ty -> 
	match ty with
	  (_,((Ast_c.TypeName(_,_),_) as ty)) -> print_typedef pr typedefs ty
	| _ -> k ty) } in
  List.iter
    (function (_,vl) ->
      match vl with
	Ast_c.MetaIdVal(_) | Ast_c.MetaFuncVal(_)
      | Ast_c.MetaLocalFuncVal(_) -> ()
      | Ast_c.MetaExprVal(exp) -> Visitor_c.vk_expr bigf exp
      | Ast_c.MetaExprListVal(args) -> Visitor_c.vk_argument_list bigf args
      | Ast_c.MetaParamVal(param) -> Visitor_c.vk_param bigf param
      | Ast_c.MetaParamListVal(params) -> Visitor_c.vk_param_list bigf params

      | Ast_c.MetaTypeVal(ty) -> Visitor_c.vk_type bigf ty
      | Ast_c.MetaStmtVal(stm) -> Visitor_c.vk_statement bigf stm
      | Ast_c.MetaPosVal _ | Ast_c.MetaPosValList _
      | Ast_c.MetaListlenVal _ -> ())
    env

let print_types pr = function
    None -> ()
  | Some [ty] -> pr (Type_cocci.type2c ty)
  | Some types ->
      pr "{";
      Common.print_between (function _ -> pr ", ")
	(function ty -> pr (Type_cocci.type2c ty)) types;
      pr "}"

let pp_meta_decl pr decl =
  let no_arity = function Ast.NONE -> () | _ -> failwith "no arity allowed" in
  let pp_name (_,n) = pr n in
  match decl with
    Ast.MetaIdDecl(ar, name) ->
      no_arity ar; pr "identifier "; pp_name name; pr ";\n"
  | Ast.MetaFreshIdDecl(ar, name) ->
      no_arity ar; pr "fresh identifier "; pp_name name; pr ";\n"
  | Ast.MetaTypeDecl(ar, name) ->
      no_arity ar; pr "type "; pp_name name; pr ";\n"
  | Ast.MetaListlenDecl(name) -> ()
  | Ast.MetaParamDecl(ar, name) ->
      no_arity ar; pr "parameter "; pp_name name; pr ";\n"
  | Ast.MetaParamListDecl(ar, name, None) ->
      no_arity ar; pr "parameter list "; pp_name name; pr ";\n"
  | Ast.MetaParamListDecl(ar, name, Some len) ->
      no_arity ar; pr "parameter list "; pp_name name;
      pr "["; pp_name len; pr "]"; pr ";\n"
  | Ast.MetaConstDecl(ar, name, types) ->
      no_arity ar; pr "constant "; print_types pr types;
      pp_name name; pr ";\n"
  | Ast.MetaErrDecl(ar, name) ->
      no_arity ar; pr "error "; pp_name name; pr ";\n"
  | Ast.MetaExpDecl(ar, name, None) ->
      no_arity ar; pr "expression "; pp_name name; pr ";\n"
  | Ast.MetaExpDecl(ar, name, types) ->
      no_arity ar; print_types pr types; pp_name name; pr ";\n"
  | Ast.MetaIdExpDecl(ar, name, types) ->
      no_arity ar; pr "idexpression ";
      print_types pr types; pp_name name; pr ";\n"
  | Ast.MetaLocalIdExpDecl(ar, name, types) ->
      no_arity ar; pr "local idexpression ";
      print_types pr types; pp_name name; pr ";\n"
  | Ast.MetaExpListDecl(ar, name, None) ->
      no_arity ar; pr "parameter list "; pp_name name; pr ";\n"
  | Ast.MetaExpListDecl(ar, name, Some len) ->
      no_arity ar; pr "parameter list ";
      pp_name name; pr "["; pp_name len; pr "]"; pr ";\n"
  | Ast.MetaStmDecl(ar, name) ->
      no_arity ar; pr "statement "; pp_name name; pr ";\n"
  | Ast.MetaStmListDecl(ar, name) ->
      no_arity ar; pr "statement list "; pp_name name; pr ";\n"
  | Ast.MetaFuncDecl(ar, name) ->
      no_arity ar; pr "function "; pp_name name; pr ";\n"
  | Ast.MetaLocalFuncDecl(ar, name) ->
      no_arity ar; pr "local function "; pp_name name; pr ";\n"
  | Ast.MetaPosDecl(ar, name) ->
      no_arity ar; pr "position "; pp_name name; pr ";\n"
  | Ast.MetaDeclarerDecl(ar, name) ->
      no_arity ar; pr "declarer "; pp_name name; pr ";\n"
  | Ast.MetaIteratorDecl(ar, name) ->
      no_arity ar; pr "iterator "; pp_name name; pr ";\n"

let print_metavariables pr local_metas paramst env header_req =
  (if header_req
  then pr "@depends on header@\n"
  else pr "@@\n");
  pr "position _p!=same.p;\n";
  pr "identifier _f;\n";
  let typedefs = ref ([] : string list) in
  let rec loop = function
      [] | [(((_,_,(_,(Ast_c.BaseType(Ast_c.Void),_))),_),_)] -> []
    | ((first,_) as f)::rest ->
	print_metavar pr typedefs first; pr ";\n";
	(make_exp f) :: loop rest in
  let args = loop paramst in
  print_extra_typedefs pr typedefs env;
  List.iter (pp_meta_decl pr) local_metas;
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
    ((_,Some param,_),_) -> pr param
  | _ -> failwith "function must have named parameters"

let pp_def_gen pr defn isexp =
  let {Ast_c.f_name = s; f_type = (_, (paramst, (b, iib))); } = defn in
  pr s; pr "(";
  (if b then failwith "not handling variable argument functions");
  (match paramst with
    [] | [(((_,_,(_,(Ast_c.BaseType(Ast_c.Void),_))),_),_)] -> ()
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
	   Ast.DECL(s) -> [[Ast.StatementTag s]]
	 | Ast.CODE(ss) -> [[Ast.StmtDotsTag ss]]
	 | _ -> error body "bad rule body")
    | _ -> failwith "bad rule" in
  (* create the output file *)
  let outdir =
    match !Flag.make_hrule with
      Some outdir -> outdir
    | None -> error rule "not possible" in
  let function_name = get_function_name rule env in
  let outfile = outdir ^ "/" ^ function_name in
  let outfile =
    try
      let cell = List.assoc outfile !names in
      let ct = !cell in
      cell := ct + 1;
      outfile ^ (string_of_int ct)
    with Not_found ->
      let cell = ref 1 in names := (outfile,cell) :: !names; outfile in
  let outfile = outfile ^ ".cocci" in
  Common.with_open_outfile outfile (fun (pr,chan) ->
    let header_req = print_header_rule pr srcfile in
    print_check_rule pr function_name header_req;
    let args = print_metavariables pr local_metas paramst env header_req in
    let env = (args_name,Ast_c.MetaExprListVal args)::env in
    print_start pr;
    (* for printing C tokens *)
    let pr_c info =
      match Ast_c.pinfo_of_info info with
	Ast_c.AbstractLineTok _ -> pr (Ast_c.str_of_info info)
      | Ast_c.FakeTok (s,_) -> pr s
      |	_ ->
	  Printf.printf "line: %s\n" (Common.dump info);
	  error rule "not an abstract line" in
    Unparse_cocci.pp_list_list_any
      (env, pr, pr_c, (function _ -> pr " "),
       (function _ -> ()), (function _ -> ()))
      true printable Unparse_cocci.InPlace;
    print_end pr;
    pr "\n")
