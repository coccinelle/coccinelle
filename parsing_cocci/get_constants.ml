(* get a list of all of the constants in the - slice of a SmPL file, to be
used to select which files to process *)

module Ast0 = Ast0_cocci
module V0 = Visitor_ast0

let get_constants rules =
  let donothing r k e = k e in
  let bind x y = Common.union_set x y in
  let option_default = [] in
  let mcode _ = option_default in

  let ident r k e =
    match Ast0.unwrap e with
      Ast0.Id(name) -> [Ast0.unwrap_mcode name]
    | _ -> k e in

  let expression r k e =
    match Ast0.unwrap e with
      Ast0.RecordAccess(exp,_,fld) | Ast0.RecordPtAccess(exp,_,fld) ->
	bind
	  (List.concat
	     (List.map (function id -> ["."^id;"->"^id])
		(r.V0.combiner_ident fld)))
	  (r.V0.combiner_expression exp)
    | _ -> k e in

  let res =
    V0.combiner bind option_default
      mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode mcode
      donothing donothing donothing
      ident expression donothing donothing donothing donothing donothing in

  let rule_fn tls =
    List.fold_left
      (function rest ->
	function cur ->
	  Common.union_set (res.V0.combiner_top_level cur) rest)
      [] tls in
      
  List.fold_left
    (function rest -> function cur -> Common.union_set (rule_fn cur) rest)
    [] rules
