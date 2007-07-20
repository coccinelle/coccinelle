(* get a list of all of the constants in the - slice of a SmPL file, to be
used to select which files to process *)

(* This could be made more efficient, by finding only the important things.
eg, if we have a function and its arguments, we could just pick the function.
And we could try to pick only the things annotated with -, and only pick
something else if there is no -.  In general, we only want the most important
constant, not all the constants. *)

module Ast = Ast_cocci
module V = Visitor_ast

let keep_some_bind x y = match x with [] -> y | _ -> x
let keep_all_bind = Common.union_set

let get_minus_constants bind =
  let donothing r k e = k e in
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

  let rule_elem r k e =
    match Ast.unwrap e with
      Ast.DisjRuleElem(res) ->
	union_all (List.map r.V.combiner_rule_elem res)
    | _ -> k e in

  let statement r k e =
    match Ast.unwrap e with
      Ast.Disj(stmt_dots) ->
	union_all (List.map r.V.combiner_statement_dots stmt_dots)
    | _ -> k e in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing donothing
    ident expression fullType typeC donothing donothing declaration
    rule_elem statement donothing donothing donothing

(* ------------------------------------------------------------------------ *)

let get_plus_constants =
  let donothing r k e = k e in
  let bind = Common.union_set in
  let option_default = [] in
  let mcode r (_,_,mcodekind) =
    let recurse l =
      List.fold_left
	(List.fold_left
	   (function prev ->
	     function cur ->
	       bind
		 ((get_minus_constants keep_all_bind).V.combiner_anything
		    cur)
		 prev))
	[] l in
    match mcodekind with
      Ast.MINUS(_,anythings) -> recurse anythings
    | Ast.CONTEXT(_,Ast.BEFORE(a)) -> recurse a
    | Ast.CONTEXT(_,Ast.AFTER(a)) -> recurse a
    | Ast.CONTEXT(_,Ast.BEFOREAFTER(a1,a2)) ->
	Common.union_set (recurse a1) (recurse a2)
    | _ -> [] in

  V.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing

(* ------------------------------------------------------------------------ *)

let rule_fn tls in_plus =
  List.fold_left
    (function (rest_info,in_plus) ->
      function cur ->
	let minuses =
	  (get_minus_constants keep_some_bind).V.combiner_top_level cur in
	let plusses = get_plus_constants.V.combiner_top_level cur in
	let new_plusses = Common.union_set plusses in_plus in
	let new_minuses = Common.minus_set minuses new_plusses in
	(Common.union_set new_minuses rest_info, new_plusses))
    ([],in_plus) tls

let get_constants rules =
  let (info,_) =
    List.fold_left
      (function (rest_info,in_plus) ->
	function (nm,dep,cur) ->
	  let (cur_info,cur_plus) = rule_fn cur in_plus in
	  (Common.union_set cur_info rest_info,cur_plus))
      ([],[]) rules in
  info
