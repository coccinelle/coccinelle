(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

(* call set_test_pos on test expressions *)

(* The goal of this is to identify test expressions in the SmPL file, so that
isomorphisms like X != NULL => X are only applied in a test expression context.

There is a related check in cocci_vs_c3.ml that in x || ..., a match
without the || is only accepted in a test expression context.  This uses
the annotations in the C file. *)

let rec process_exp e =
  let e = Ast0.set_test_pos e in(* allow test isos *)
  let e = Ast0.set_test_exp e in(* require that a test expression is matched *)
  match Ast0.unwrap e with
    Ast0.Paren(lp,e1,rp) ->
      Ast0.rewrap e (Ast0.Paren(lp,process_exp e1,rp))
  | Ast0.DisjExpr(lp,es,bars,rp) ->
      Ast0.rewrap e (Ast0.DisjExpr(lp,List.map process_exp es,bars,rp))
  | _ -> e

let set_test_poss =
  let expression r k e =
    let e = k e in
    match Ast0.unwrap e with
      Ast0.CondExpr(e1,q,e2,c,e3) ->
	Ast0.rewrap e (Ast0.CondExpr(process_exp e1,q,e2,c,e3))
    | Ast0.Binary(e1,op,e2) ->
	(match Ast0.unwrap op with
	  Ast0.Logical op' ->
	    (match Ast0.unwrap_mcode op' with
	      Ast.AndLog _ | Ast.OrLog _ -> 
		Ast0.rewrap e (Ast0.Binary(process_exp e1,op,process_exp e2))
	    | _ -> e)
	| _ -> e)
    | Ast0.Unary(e1,op) ->
	(match Ast0.unwrap_mcode op with
	  Ast.Not _ -> Ast0.rewrap e (Ast0.Unary(process_exp e1,op))
	| _ -> e)
    | _ -> e in

  let process_wc = function
      Ast0.WhenNotTrue(w,ee,e) -> Ast0.WhenNotTrue(w,ee,process_exp e)
    | Ast0.WhenNotFalse(w,ee,e) -> Ast0.WhenNotFalse(w,ee,process_exp e)
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
    | Ast0.For(f,lp,first,rp,s1,aft) ->
	(match Ast0.unwrap first with
	  Ast0.ForExp(e1a,sc1a,Some e2a,sc2a,e3a) ->
	    let first =
	      Ast0.rewrap first
		(Ast0.ForExp(e1a,sc1a,Some (process_exp e2a),sc2a,e3a)) in
	    Ast0.rewrap s (Ast0.For(f,lp,first,rp,s1,aft))
	| Ast0.ForDecl (bef,decla,Some e2a,sc2a,e3a) ->
	    let first =
	      Ast0.rewrap first
		(Ast0.ForDecl(bef,decla,Some (process_exp e2a),sc2a,e3a)) in
	    Ast0.rewrap s (Ast0.For(f,lp,first,rp,s1,aft))
	| _ -> s)
    | Ast0.Dots(d,wc) ->
	Ast0.rewrap s (Ast0.Dots(d,List.map process_wc wc))
    | Ast0.Nest(l,s1,r,wc,m) ->
	Ast0.rewrap s (Ast0.Nest(l,s1,r,List.map process_wc wc,m))
    | _ -> s in

  V0.rebuilder_default ~expr:expression ~stmt:statement ()

let process = List.map set_test_poss.VT0.rebuilder_rec_top_level

let process_anything = set_test_poss.VT0.rebuilder_rec_anything
