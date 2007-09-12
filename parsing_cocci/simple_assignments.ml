module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0

(* find assignments that can match an initialization *)

let pure_mcodekind = function
    Ast0.CONTEXT(mc) ->
      (match !mc with
	(Ast.NOTHING,_,_) -> true
      |	_ -> false)
  | _ -> false

let is_pure_id id =
  (pure_mcodekind (Ast0.get_mcodekind id)) &&
  (* no subterms, so if pure at toplevel, pure further in as well *)
  (match Ast0.unwrap id with
    Ast0.Ident(_) | Ast0.MetaExpr(_,_,_,_) -> true
  | _ -> false)

let is_pure_assign op =
  ((Ast0.unwrap_mcode op) = Ast.SimpleAssign) &&
  (pure_mcodekind (Ast0.get_mcode_mcodekind op))

let simple_assignments l =
  let mcode x = x in
  let donothing r k e = k e in
  let statement r k e =
    match Ast0.unwrap e with
      Ast0.Exp(e1) ->
	(match Ast0.unwrap e1 with
	  Ast0.Assignment(left,op,right,_) ->
	    let simple =
	      (pure_mcodekind (Ast0.get_mcodekind e)) &&
	      (is_pure_id left) &&
	      (is_pure_assign op) in
	    Ast0.rewrap e
	      (Ast0.Exp
		 (Ast0.rewrap e1 (Ast0.Assignment(left,op,right,simple))))
	| _ -> k e)
    | _ -> k e in
  let fn =
    V0.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing statement
      donothing donothing in
  List.map fn.V0.rebuilder_top_level l
