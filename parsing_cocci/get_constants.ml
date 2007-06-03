(* get a list of all of the constants in the - slice of a SmPL file, to be
used to select which files to process *)

(* This could be made more efficient, by finding only the important things.
eg, if we have a function and its arguments, we could just pick the function.
And we could try to pick only the things annotated with -, and only pick
something else if there is no -.  In general, we only want the most important
constant, not all the constants. *)

module Ast = Ast_cocci
module V = Visitor_ast

let get_constants rules =
  let donothing r k e = k e in
  let bind x y = match x with [] -> y | _ -> x in
  let option_default = [] in
  let mcode _ _ = option_default in

  let rec union_all = function
      [] -> []
    | x::xs -> Common.union_set x (union_all xs) in

  (* need special cases for everything with a disj, because the bind above
     would throw away all but the first disj *)

  let ident r k e =
    match Ast.unwrap e with
      Ast.Id(name) ->
	(match Ast.unwrap_mcode name with
	  "NULL" -> [] (* special case, because this is too generic *)
	| nm ->
	    if !Flag_parsing_cocci.sgrep_mode2
	    then
	      match name with
		(_,_,Ast.MINUS(_,_)) -> [nm]
	      |	_ -> []
	    else [nm])
    | _ -> k e in

  let expression r k e =
    match Ast.unwrap e with
      Ast.RecordAccess(exp,_,fld) | Ast.RecordPtAccess(exp,_,fld) ->
	bind
	  (union_all
	     (List.map (function id -> ["."^id;"->"^id])
		(r.V.combiner_ident fld)))
	  (r.V.combiner_expression exp)
    | Ast.SizeOfExpr(sizeof,_) | Ast.SizeOfType(sizeof,_,_,_) ->
	bind (k e) [Ast.unwrap_mcode sizeof]
    | Ast.DisjExpr(exps) -> union_all (List.map r.V.combiner_expression exps)
    | _ -> k e in

  let typeC r k e =
    match Ast.unwrap e with
      Ast.TypeName(ty) ->
	if !Flag_parsing_cocci.sgrep_mode2
	then
	  match ty with
	    (_,_,Ast.MINUS(_,_)) -> [Ast.unwrap_mcode ty]
	  | _ -> []
	else [Ast.unwrap_mcode ty]
    | _ -> k e in

  let fullType r k e =
    match Ast.unwrap e with
      Ast.DisjType(types) -> union_all (List.map r.V.combiner_fullType types)
    | _ -> k e in

  let declaration r k e =
    match Ast.unwrap e with
      Ast.DisjDecl(decls) ->
	union_all (List.map r.V.combiner_declaration decls)
    | _ -> k e in

  let statement r k e =
    match Ast.unwrap e with
      Ast.Disj(stmt_dots) ->
	union_all (List.map r.V.combiner_statement_dots stmt_dots)
    | _ -> k e in

  let res =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode
      donothing donothing donothing donothing
      ident expression fullType typeC donothing donothing declaration
      donothing statement donothing donothing donothing in

  let rule_fn tls =
    List.fold_left
      (function rest ->
	function cur ->
	  Common.union_set (res.V.combiner_top_level cur) rest)
      [] tls in
      
  List.fold_left
    (function rest ->
      function (nm,dep,cur) ->
	Common.union_set (rule_fn cur) rest)
    [] rules
