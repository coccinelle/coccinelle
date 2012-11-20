module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

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
	  Ast0.IfThen(iff,lp,exp,rp,branch,(info,mc,!counter))
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(info,mc,_)) ->
	  Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(info,mc,!counter))
      | Ast0.While(wh,lp,exp,rp,body,(info,mc,_)) ->
	  Ast0.While(wh,lp,exp,rp,body,(info,mc,!counter))
      | Ast0.For(fr,lp,first,exp2,sem2,exp3,rp,body,(info,mc,_)) ->
	  Ast0.For(fr,lp,first,exp2,sem2,exp3,rp,body,(info,mc,!counter))
      | Ast0.Iterator(nm,lp,args,rp,body,(info,mc,_)) ->
	  Ast0.Iterator(nm,lp,args,rp,body,(info,mc,!counter))
      | s -> s) in
  let fn =
    V0.rebuilder
      {V0.rebuilder_functions with
	VT0.rebuilder_meta_mcode = mcode;
	VT0.rebuilder_string_mcode = string_mcode;
	VT0.rebuilder_const_mcode = mcode;
	VT0.rebuilder_assign_mcode = mcode;
	VT0.rebuilder_fix_mcode = mcode;
	VT0.rebuilder_unary_mcode = mcode;
	VT0.rebuilder_binary_mcode = mcode;
	VT0.rebuilder_cv_mcode = mcode;
	VT0.rebuilder_sign_mcode = mcode;
	VT0.rebuilder_struct_mcode = mcode;
	VT0.rebuilder_storage_mcode = mcode;
	VT0.rebuilder_inc_mcode = mcode;
	VT0.rebuilder_stmtfn = statement;} in
  List.map fn.VT0.rebuilder_rec_top_level p

