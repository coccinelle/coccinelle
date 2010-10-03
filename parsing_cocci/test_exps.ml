(*
* Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
* Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
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


module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0

(* call set_test_pos on test expressions *)

(* The goal of this is to identify test expressions in the SmPL file, so that
isomorphisms like X != NULL => X are only applied in a test expression context.

There is a related check in cocci_vs_c3.ml that in x || ..., a match
without the || is only accepted in a test expression context.  This uses
the annotations in the C file. *)

let rec process_exp e =
  let e = Ast0.set_test_pos e in
  match Ast0.unwrap e with
    Ast0.Paren(lp,e,rp) ->
      Ast0.rewrap e (Ast0.Paren(lp,process_exp e,rp))
  | _ -> e

let set_test_poss =
  let donothing r k e = k e in
  let mcode x = x in

  let expression r k e =
    let e = k e in
    match Ast0.unwrap e with
      Ast0.CondExpr(e1,q,e2,c,e3) ->
	Ast0.rewrap e (Ast0.CondExpr(process_exp e1,q,e2,c,e3))
    | Ast0.Binary(e1,op,e2) ->
	(match Ast0.unwrap_mcode op with
	  Ast.Logical(Ast.AndLog) | Ast.Logical(Ast.OrLog) ->
	    Ast0.rewrap e (Ast0.Binary(process_exp e1,op,process_exp e2))
	| _ -> e)
    | Ast0.Unary(e1,op) ->
	(match Ast0.unwrap_mcode op with
	  Ast.Not -> Ast0.rewrap e (Ast0.Unary(process_exp e1,op))
	| _ -> e)
    | _ -> e in

  let process_wc = function
      Ast0.WhenNotTrue(e) -> Ast0.WhenNotTrue(process_exp e)
    | Ast0.WhenNotFalse(e) -> Ast0.WhenNotFalse(process_exp e)
    | wc -> wc in

  let statement r k s =
    let s = k s in
    match Ast0.unwrap s with
      Ast0.IfThen(i,lp,e,rp,s1,aft) ->
	Ast0.rewrap s (Ast0.IfThen(i,lp,process_exp e,rp,s1,aft))
    | Ast0.IfThenElse(i,lp,e,rp,s1,e1,s2,aft) ->
	Ast0.rewrap s (Ast0.IfThenElse(i,lp,process_exp e,rp,s1,e1,s2,aft))
    | Ast0.While(i,lp,e,rp,s1,aft) ->
	Ast0.rewrap s (Ast0.While(i,lp,process_exp e,rp,s1,aft))
    | Ast0.Do(d,s1,w,lp,e,rp,sc) ->
	Ast0.rewrap s (Ast0.Do(d,s1,w,lp,process_exp e,rp,sc))
    | Ast0.For(f,lp,e1,sc1,Some e2,sc2,e3,rp,s1,aft) ->
	Ast0.rewrap s
	  (Ast0.For(f,lp,e1,sc1,Some (process_exp e2),sc2,e3,rp,s1,aft))
    | Ast0.Dots(d,wc) ->
	Ast0.rewrap s (Ast0.Dots(d,List.map process_wc wc))
    | Ast0.Nest(l,s1,r,wc,m) ->
	Ast0.rewrap s (Ast0.Nest(l,s1,r,List.map process_wc wc,m))
    | _ -> s in

  V0.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode
      donothing donothing donothing donothing donothing donothing
      donothing expression donothing donothing donothing donothing statement
      donothing donothing

let process = List.map set_test_poss.V0.rebuilder_top_level

let process_anything = set_test_poss.V0.rebuilder_anything

