module Ast = Ast_cocci
module Past = Ast_popl

(* --------------------------------------------------------------------- *)

let rec stm s inif =
  match Ast.unwrap s with
    Ast.Atomic(ast) ->
      let term =
	match Ast.unwrap ast with
	  Ast.ExprStatement(_,_) -> Past.Term ast
	| Ast.Exp(_) -> Past.Term ast
	| Ast.Decl(_,_,_) -> Past.Term ast
	| Ast.ReturnExpr(_,_,_) -> Past.Term ast
	| Ast.MetaStmt(_,Type_cocci.Unitary,_,false) when inif -> Past.Term ast
	| _ ->
	    Pretty_print_cocci.statement "" s;
	    failwith "complex statements not supported" in
      Past.Atomic(term,dots_bef_aft s inif)
  | Ast.Disj(stm1::stm2::stmts) ->
      List.fold_left
	(function prev ->
	  function cur ->
	    Past.Or(Past.Seq(prev,Past.Empty),stm_list cur))
	(Past.Or(stm_list stm1,stm_list stm2)) stmts
  | Ast.Dots(dots,whencodes,_,_) ->
      Past.DInfo
	(List.fold_left
	   (function prev ->
	     function
		 Ast.WhenNot(a) -> Past.When(prev,stm_list a)
	       | _ -> failwith "only when != supported")
	   Past.Dots whencodes)
  | Ast.Nest(stmt_dots,whencodes,false,_,_) ->
      let nest = Past.Nest(stm_list stmt_dots) in
      Past.DInfo
	(List.fold_left
	   (function prev ->
	     function
		 Ast.WhenNot(a) -> Past.When(prev,stm_list a)
	       | _ -> failwith "only when != supported")
	   nest whencodes)
  | Ast.IfThen(header,body,aft) ->
      Past.IfThen(Past.Term header,stm body true,aft,dots_bef_aft s inif)
  | _ ->
      Pretty_print_cocci.statement "" s;
      failwith "unsupported statement3"

and dots_bef_aft s inif =
  match Ast.get_dots_bef_aft s with
    Ast.AddingBetweenDots (brace_term,n) ->
      Past.AddingBetweenDots (stm brace_term inif,n)
  | Ast.DroppingBetweenDots (brace_term,n) ->
      Past.DroppingBetweenDots (stm brace_term inif,n)
  | Ast.NoDots -> Past.NoDots

and stm_list s =
  match Ast.unwrap s with
    Ast.DOTS(d) ->
      List.fold_right
	(function cur -> function rest -> Past.Seq(stm cur false, rest))
	d Past.Empty
  | _ -> failwith "only DOTS handled"

let top s =
  match Ast.unwrap s with
    Ast.CODE(stmt_dots) -> stm_list stmt_dots
  | _ -> failwith "only CODE handled"
