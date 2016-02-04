(*
 * Copyright 2012-2015, Inria
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./asttopopl.ml"
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
      | Ast.MetaStmt(_,_,_,_) when inif -> Past.Atomic ast
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
  match Ast.unwrap s with
    Ast.DOTS(d) ->
      List.fold_right
	(function cur -> function rest -> Past.Seq(stm cur, rest))
	d Past.Empty
  | _ -> failwith "only DOTS handled"

let top s =
  match Ast.unwrap s with
    Ast.CODE(stmt_dots) -> stm_list stmt_dots
  | _ -> failwith "only CODE handled"
