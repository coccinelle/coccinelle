(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Goal: Move a MetaStmtList, if any, up into { and replace by ... *)
(* Only applied to - code *)

module Ast = Ast_cocci
module V = Visitor_ast

let adjust_brace_and_body lbrace body =
  let body_element =
    match Ast.unwrap body with
      [x] ->
	(match Ast.unwrap x with
	  Ast.Atomic(y) ->
	    (match Ast.unwrap y with
	      Ast.MetaStmtList(name,lenname,cstr,keep,inherited) ->
		Some(name,lenname,cstr,keep,inherited,body,x,y)
	    | _ -> None)
	| _ -> None)
    | _ -> None in
  match body_element with
    Some(name,lenname,cstr,keep,inherited,body,x,y) ->
      let newbody =
	(* dropping x *)
	Ast.rewrap y (Ast.Dots(Ast.rewrap_mcode name "...",[],[],[])) in
      let name = Ast.make_mcode(Ast.unwrap_mcode(name)) in
      let meta =
	Ast.make_term (Ast.MetaStmtList(name,lenname,cstr,keep,inherited)) in
      let body = Ast.rewrap body [newbody] in
      (Ast.make_term(Ast.AsRe(lbrace,meta)),body)
  | None -> (lbrace,body)

let statement r k s =
  let s = k s in
  match Ast.unwrap s with
    Ast.Seq(lbrace,body,rbrace) ->
      let (lbrace,body) = adjust_brace_and_body lbrace body in
      Ast.rewrap s (Ast.Seq(lbrace,body,rbrace))
  | Ast.FunDecl(header,lbrace,body,rbrace,endinfo) ->
      let (lbrace,body) = adjust_brace_and_body lbrace body in
      Ast.rewrap s (Ast.FunDecl(header,lbrace,body,rbrace,endinfo))
  | _ -> s

let mcode mc = mc
let donothing r k e = k e

let stmtlist_rebuilder =
  V.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing
    statement donothing donothing donothing donothing donothing

let stmtlist rule =
  match rule with
    Ast.ScriptRule _ | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ -> rule
  | Ast.CocciRule (nm, rule_info, r, is_exp,ruletype) ->
      let r = List.map stmtlist_rebuilder.V.rebuilder_top_level r in
      Ast.CocciRule (nm, rule_info, r, is_exp,ruletype)
