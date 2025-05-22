(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

(* Negate counter of open brace when that is minus and preceding ) is context.
   In that case, the brace should somehow be like the preceding ) (from an
   if, while, etc) rather than like the rest of the statement. *)
let redo (_,_,_,mc,_,_) stm =
  match mc with
    Ast0.CONTEXT _ ->
      (match Ast0.unwrap stm with
	Ast0.Seq((a,b,c,mc_lbrace,d,adj),body,rbrace) ->
	  (match mc_lbrace with
	    Ast0.MINUS _ ->
	      (* subtract 1 in case -1 is used for some unknown values *)
	      let adj = { adj with Ast.counter = -1 * adj.Ast.counter - 1 } in
	      Ast0.rewrap stm (Ast0.Seq((a,b,c,mc_lbrace,d,adj),body,rbrace))
	  | _ -> stm)
      |	_ -> stm)
  | _ -> stm

let compute_adjacency p =
  let prev = ref { Ast.counter = 0; Ast.ender = false } in
  let counter = ref 0 in
  let get_adj _ =
    let adj = { Ast.counter = !counter; Ast.ender = false } in
    prev := adj;
    adj in
  let mcode (a,b,c,d,e,_) = (a,b,c,d,e,get_adj()) in
  let string_mcode ((str,_,info,mc,_,_) as x) =
    match str with
      "..." | "<..." | "...>" | "<+..." | "...+>" ->
	(!prev).Ast.ender <- true;
	(match mc with
	  Ast0.MINUS _ -> mcode x
	| Ast0.CONTEXT _ -> counter := !counter + 1; x
	| _ -> failwith "unexpected mcode for ...")
    | _ -> mcode x in
  let statement r k s =
    let s = k s in
    (* a case for each kind of term that has a fake node *)
    Ast0.rewrap s
      (match Ast0.unwrap s with
	Ast0.IfThen(iff,lp,exp,rp,branch,(info,mc,_)) ->
	  let branch = redo rp branch in
	  Ast0.IfThen(iff,lp,exp,rp,branch,(info,mc,get_adj()))
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(info,mc,_)) ->
	  let branch1 = redo rp branch1 in
	  let branch2 = redo els branch2 in
	  let adj = { Ast.counter = !counter; Ast.ender = false } in
	  prev := adj;
	  Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(info,mc,get_adj()))
      | Ast0.While(wh,lp,exp,rp,body,(info,mc,_)) ->
	  let body = redo rp body in
	  let adj = { Ast.counter = !counter; Ast.ender = false } in
	  prev := adj;
	  Ast0.While(wh,lp,exp,rp,body,(info,mc,get_adj()))
      | Ast0.For(fr,lp,first,rp,body,(info,mc,_)) ->
	  let body = redo rp body in
	  let adj = { Ast.counter = !counter; Ast.ender = false } in
	  prev := adj;
	  Ast0.For(fr,lp,first,rp,body,(info,mc,get_adj()))
      | Ast0.ScopedGuard(sg,lp,exp,rp,body,(info,mc,_)) ->
	  let body = redo rp body in
	  let adj = { Ast.counter = !counter; Ast.ender = false } in
	  prev := adj;
	  Ast0.ScopedGuard(sg,lp,exp,rp,body,(info,mc,get_adj()))
      | Ast0.Iterator(nm,lp,args,rp,body,(info,mc,_)) ->
	  let body = redo rp body in
	  let adj = { Ast.counter = !counter; Ast.ender = false } in
	  prev := adj;
	  Ast0.Iterator(nm,lp,args,rp,body,(info,mc,get_adj()))
      | s -> s) in
  let fn =
    V0.rebuilder {V0.rmcode=mcode} {V0.rdonothing=(fun r k e -> k e)}
      ~string_mcode:string_mcode ~stmt:statement () in
  List.map fn.VT0.rebuilder_rec_top_level p
