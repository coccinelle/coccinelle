(*
 * Copyright 2012-2014, INRIA
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


# 0 "./safe_for_multi_decls.ml"
(* This phase sets the safe_for_multi_decls field, which is normally false,
to true for transformations on declarations where the only change is on the
declared variable.  This is the only kind of change on such a declaration
that can safely be done without splitting the declaration.

This also now allows complete removal of a declaration, with no changes
inside.  This is related to the danger data type and the code in
parsing_c/danger.ml *)

module Ast = Ast_cocci
module V = Visitor_ast

(* ------------------------------------------------------------------------- *)
(* check if everything is removed, with no additions allowed *)

let all_removed =
  let bind x y = x && y in
  let option_default = true in
  let do_nothing r k e = k e in
  let mcode _ (_,_,kind,_) =
    match kind with
      Ast.MINUS(_,_,_,Ast.NOREPLACEMENT) -> true
    | Ast.MINUS(_,_,_,_) -> false
    | Ast.PLUS _ -> failwith "not possible"
    | Ast.CONTEXT(_,info) -> false in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing in
  recursor.V.combiner_declaration

(* ------------------------------------------------------------------------- *)

let mcode _ (_,_,kind,_) =
  match kind with
    Ast.MINUS(_,_,_,_) -> true
  | Ast.PLUS _ -> failwith "not possible"
  | Ast.CONTEXT(_,info) -> not (info = Ast.NOTHING)

let contains_modif =
  let bind x y = x or y in
  let option_default = false in
  let do_nothing r k e = k e in
  let annotated_decl decl =
    match Ast.unwrap decl with
      Ast.DElem(bef,_,_) -> bef
    | _ -> failwith "not possible" in
  let rule_elem r k re =
    (* Very obscure how this could arise.  Array type contains an expression
       and the expression could contain a statement. *)
    let res = k re in
    match Ast.unwrap re with
      Ast.FunHeader(bef,_,fninfo,name,lp,params,rp) ->
	bind (mcode r ((),(),bef,[])) res
    | Ast.Decl decl ->
	bind (mcode r ((),(),annotated_decl decl,[])) res
    | Ast.ForHeader(fr,lp,Ast.ForDecl(decl),e2,sem2,e3,rp) ->
	bind (mcode r ((),(),annotated_decl decl,[])) res
    | _ -> res in
  let init r k i =
    let res = k i in
    match Ast.unwrap i with
      Ast.StrInitList(allminus,_,_,_,_) -> allminus or res
    | _ -> res in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing init do_nothing do_nothing
      do_nothing rule_elem do_nothing do_nothing do_nothing do_nothing in
  recursor.V.combiner_fullType

let decl r k e =
  let e = k e in
  if all_removed e
  then {e with Ast.safe_for_multi_decls = true}
  else
    match Ast.unwrap e with
      Ast.Init(stg,ty,_,_,_,sem)
    | Ast.UnInit(stg,ty,_,sem) ->
	let stg_modif =
	  match stg with
	    Some stg -> mcode () stg
	  | None -> false in
	let ft_modif = contains_modif ty in
	let sem_modif = mcode () sem in
	if not(stg_modif or ft_modif or sem_modif)
	then {e with Ast.safe_for_multi_decls = true}
	else e
    | _ -> e
let mcode e = e
let donothing r k e = k e

let process =
  let fn = V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing
      donothing donothing decl donothing donothing
      donothing donothing donothing donothing in
  List.map fn.V.rebuilder_top_level

let safe_for_multi_decls rules =
  List.map
    (function (mv,r) ->
      (mv,
       match r with
        Ast.ScriptRule _
      | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ -> r
      | Ast.CocciRule (nm, rule_info, r, is_exp,ruletype) ->
	  Ast.CocciRule(nm, rule_info,process r,is_exp,ruletype)))
    rules
