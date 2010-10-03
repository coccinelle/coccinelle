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
	VT0.rebuilder_inc_mcode = mcode;} in
  List.map fn.VT0.rebuilder_rec_top_level p

