(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

module Ast = Ast_cocci
module Past = Ast_popl

(* --------------------------------------------------------------------- *)

let term s inif =
  let fail _ =
    Pretty_print_cocci.statement "" s;
    Format.print_newline();
    failwith "complex statements not supported" in
  match Ast.unwrap s with
    Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.ExprStatement(_,_) -> Past.Atomic ast
      | Ast.Exp(_) -> Past.Atomic ast
      | Ast.Decl(_) -> Past.Atomic ast
      | Ast.ReturnExpr(_,_,_) -> Past.Atomic ast
      | Ast.MetaStmt(_,_,_,_,_) when inif -> Past.Atomic ast
      | Ast.DisjRuleElem(_) -> Past.Atomic ast
      | _ -> fail())
  | _ -> fail()

let rec stm s =
  match Ast.unwrap s with
    Ast.Atomic(ast) -> Past.Term(term s false,dots_bef_aft s false)
  | Ast.IfThen(header,body,aft) ->
      Past.Term(
      Past.IfThen(Past.Atomic header,term body true,aft),
      dots_bef_aft s true)
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
  | Ast.Nest(starter,stmt_dots,ender,whencodes,false,_,_) ->
      (match Ast.get_mcodekind starter with
	Ast.MINUS _ -> failwith "only context nests supported"
      |	_ -> ());
      let nest = Past.Nest(stm_list stmt_dots) in
      Past.DInfo
	(List.fold_left
	   (function prev ->
	     function
		 Ast.WhenNot(a) -> Past.When(prev,stm_list a)
	       | _ -> failwith "only when != supported")
	   nest whencodes)
  | _ ->
      Pretty_print_cocci.statement "" s;
      failwith "unsupported statement3"

and dots_bef_aft s inif =
  match Ast.get_dots_bef_aft s with
    Ast.AddingBetweenDots (brace_term,n) ->
      Past.AddingBetweenDots (term brace_term inif,n)
  | Ast.DroppingBetweenDots (brace_term,n) ->
      Past.DroppingBetweenDots (term brace_term inif,n)
  | Ast.NoDots -> Past.NoDots

and stm_list s =
  List.fold_right
    (function cur -> function rest -> Past.Seq(stm cur, rest))
    (Ast.unwrap s) Past.Empty

let top s =
  match Ast.unwrap s with
    Ast.CODE(stmt_dots) -> stm_list stmt_dots
  | _ -> failwith "only CODE handled"
