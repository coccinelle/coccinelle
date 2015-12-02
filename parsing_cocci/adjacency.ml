(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

(* Negate counter of open brace when that is minus and preceding ) is context.
   In that case, the brace should somehow be like the preceding ) (from an
   if, while, etc) rather than like the rest of the statement. *)
let redo (_,_,_,mc,_,_) stm =
  match mc with
    Ast0.CONTEXT _ ->
      (match Ast0.unwrap stm with
	Ast0.Seq((a,b,c,mc_lbrace,d,ctr),body,rbrace) ->
	  (match mc_lbrace with
	    Ast0.MINUS _ ->
	      (* subtract 1 in case -1 is used for some unknown values *)
	      let ctr = -1 * ctr - 1 in
	      Ast0.rewrap stm (Ast0.Seq((a,b,c,mc_lbrace,d,ctr),body,rbrace))
	  | _ -> stm)
      |	_ -> stm)
  | _ -> stm

let compute_adjacency p =
  let counter = ref 0 in
  let mcode (a,b,c,d,e,_) = (a,b,c,d,e,!counter) in
  let string_mcode ((str,_,info,mc,_,_) as x) =
    match str with
      "..." | "<..." | "...>" | "<+..." | "...+>" ->
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
	  Ast0.IfThen(iff,lp,exp,rp,branch,(info,mc,!counter))
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(info,mc,_)) ->
	  let branch1 = redo rp branch1 in
	  let branch2 = redo els branch2 in
	  Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(info,mc,!counter))
      | Ast0.While(wh,lp,exp,rp,body,(info,mc,_)) ->
	  let body = redo rp body in
	  Ast0.While(wh,lp,exp,rp,body,(info,mc,!counter))
      | Ast0.For(fr,lp,first,exp2,sem2,exp3,rp,body,(info,mc,_)) ->
	  let body = redo rp body in
	  Ast0.For(fr,lp,first,exp2,sem2,exp3,rp,body,(info,mc,!counter))
      | Ast0.Iterator(nm,lp,args,rp,body,(info,mc,_)) ->
	  let body = redo rp body in
	  Ast0.Iterator(nm,lp,args,rp,body,(info,mc,!counter))
      | s -> s) in
  let fn =
    V0.rebuilder
      {V0.rebuilder_functions with
	VT0.rebuilder_meta_mcode = mcode;
	VT0.rebuilder_string_mcode = string_mcode;
	VT0.rebuilder_const_mcode = mcode;
	VT0.rebuilder_simpleAssign_mcode = mcode;
	VT0.rebuilder_opAssign_mcode = mcode;
	VT0.rebuilder_fix_mcode = mcode;
	VT0.rebuilder_unary_mcode = mcode;
	VT0.rebuilder_arithOp_mcode = mcode;
	VT0.rebuilder_logicalOp_mcode = mcode;
	VT0.rebuilder_cv_mcode = mcode;
	VT0.rebuilder_sign_mcode = mcode;
	VT0.rebuilder_struct_mcode = mcode;
	VT0.rebuilder_storage_mcode = mcode;
	VT0.rebuilder_inc_mcode = mcode;
	VT0.rebuilder_stmtfn = statement;} in
  List.map fn.VT0.rebuilder_rec_top_level p
