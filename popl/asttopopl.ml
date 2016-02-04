(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast = Ast_cocci
module Past = Ast_popl

(* --------------------------------------------------------------------- *)

let rec stm s =
  match Ast.unwrap s with
    Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.ExprStatement(_,_) -> Past.Term ast
      |	Ast.Exp(_) -> Past.Term ast
      |	Ast.Decl(_,_,_) -> Past.Term ast
      |	_ -> failwith "complex statements not supported")
  | Ast.Disj(stm1::stm2::stmts) ->
      List.fold_left
	(function prev ->
	  function cur ->
	    Past.Or(Past.Seq(prev,Past.Empty),stm_list cur))
	(Past.Or(stm_list stm1,stm_list stm2)) stmts
  | Ast.Dots(dots,whencodes,_,_) ->
      (match whencodes with
	[Ast.WhenNot(a)] -> Past.DInfo(Past.When(Past.Dots,stm_list a),[],[])
      |	_ -> failwith "only one when != supported")
  | Ast.Nest(stmt_dots,whencodes,false,_,_) ->
      let nest = Past.Nest(stm_list stmt_dots) in
      (match whencodes with
	[Ast.WhenNot(a)] -> Past.DInfo(Past.When(nest,stm_list a),[],[])
      |	_ -> failwith "only when != supported")
  | Ast.While(header,body,(_,_,_,aft)) | Ast.For(header,body,(_,_,_,aft)) ->
      (* only allowed if only the header is significant *)
      (match (Ast.unwrap body,aft) with
	(Ast.Atomic(re),Ast.CONTEXT(_,Ast.NOTHING)) ->
	  (match Ast.unwrap re with
	    Ast.MetaStmt(_,Type_cocci.Unitary,_,false) -> Past.Term header
	  | _ -> failwith "unsupported statement1")
      | _ -> failwith "unsupported statement2")
  | _ ->
      Pretty_print_cocci.statement "" s;
      failwith "unsupported statement3"

and stm_list s =
  List.fold_right
    (function cur -> function rest -> Past.Seq(stm cur, rest))
    (Ast.unwrap s) Past.Empty

let top s =
  match Ast.unwrap s with
    Ast.CODE(stmt_dots) -> stm_list stmt_dots
  | _ -> failwith "only CODE handled"
