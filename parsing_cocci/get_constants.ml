(* get a list of all of the constants in the - slice of a SmPL file, to be
used to select which files to process *)

module Ast = Ast_cocci
module V = Visitor_ast

let get_constants rules =
  let donothing r k e = k e in
  let bind x y = Common.union_set x y in
  let option_default = [] in
  let mcode _ _ = option_default in

  let ident r k e =
    match Ast.unwrap e with
      Ast.Id(name) -> [Ast.unwrap_mcode name]
    | _ -> k e in

  let expression r k e =
    match Ast.unwrap e with
      Ast.RecordAccess(exp,_,fld) | Ast.RecordPtAccess(exp,_,fld) ->
	bind
	  (List.concat
	     (List.map (function id -> ["."^id;"->"^id])
		(r.V.combiner_ident fld)))
	  (r.V.combiner_expression exp)
    | Ast.SizeOfExpr(sizeof,_) | Ast.SizeOfType(sizeof,_,_,_) ->
	bind [Ast.unwrap_mcode sizeof] (k e)
    | _ -> k e in

  let res =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing
      ident expression donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing in

  let rule_fn tls =
    List.fold_left
      (function rest ->
	function cur ->
	  Common.union_set (res.V.combiner_top_level cur) rest)
      [] tls in
      
  List.fold_left
    (function rest -> function cur -> Common.union_set (rule_fn cur) rest)
    [] rules
