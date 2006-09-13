(* detect statements that are between dots in the minus code, because they
may need a special treatment if they are if branches *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0

(* A very coarse approximation.  We would really only like to return true
if a new statement is added.  For this it would be best to correlate with the
plus slice. Or at least be sure that the new stuff is on the far left or
far right. *)
let rec adding_something s =
  match Ast0.get_mcodekind s with
    Ast0.MINUS(_) -> true
  | Ast0.CONTEXT(mc) ->
      let (text,tinfo1,tinfo2) = !mc in
      (match text with Ast.NOTHING -> false | _ -> true)
  | Ast0.MIXED(_) -> not(contains_only_minus s)
  | _ -> failwith "unexpected plus code"

and contains_only_minus s =
  let donothing r k e = k e in
  let bind x y = x && y in
  let option_default = true in
  let mcode (_,_,_,mc) =
    match mc with
      Ast0.MINUS(_) -> true
    | Ast0.CONTEXT(mc) ->
      let (text,tinfo1,tinfo2) = !mc in
      (match text with Ast.NOTHING -> true | _ -> false)
    | _ -> false in

  let expression r k e =
    match Ast0.unwrap e with
      Ast0.DisjExpr(starter,expr_list,mids,ender) ->
	List.for_all r.V0.combiner_expression expr_list
    | _ -> k e in

  let declaration r k e =
    match Ast0.unwrap e with
      Ast0.DisjDecl(starter,decls,mids,ender) ->
	List.for_all r.V0.combiner_declaration decls
    | _ -> k e in

  let statement r k e =
    match Ast0.unwrap e with
      Ast0.Disj(starter,statement_dots_list,mids,ender) ->
	List.for_all r.V0.combiner_statement_dots statement_dots_list
    | _ -> k e in

  let combiner = 
    V0.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing
      donothing expression donothing donothing declaration
      statement donothing in
  combiner.V0.combiner_statement s

(* ---------------------------------------------------------------------- *)

let add_braces orig_s =
  let s = (Iso_pattern.rebuild_mcode None).V0.rebuilder_statement orig_s in
  let s = Compute_lines.statement s in
  let new_mcodekind =
    match Ast0.get_mcodekind s with
      Ast0.MINUS(mc) ->
	let (text,tinfo) = !mc in
	Ast0.MINUS(ref([Ast.Token "{"]::text@[[Ast.Token "}"]],tinfo))
    | Ast0.CONTEXT(mc) ->
	let (text,tinfo1,tinfo2) = !mc in
	let new_text =
	  match text with
	    Ast.BEFORE(bef) ->
	      Ast.BEFOREAFTER([Ast.Token "{"]::bef,[[Ast.Token "}"]])
	  | Ast.AFTER(aft) ->
	      Ast.BEFOREAFTER([[Ast.Token "{"]],aft@[[Ast.Token "}"]])
	  | Ast.BEFOREAFTER(bef,aft) ->
	      Ast.BEFOREAFTER([Ast.Token "{"]::bef,aft@[[Ast.Token "}"]])
	  | Ast.NOTHING ->
	      Ast.BEFOREAFTER([[Ast.Token "{"]],[[Ast.Token "}"]]) in
	Ast0.CONTEXT(ref(new_text,tinfo1,tinfo2))
    | Ast0.MIXED(mc) ->
	let (text,tinfo1,tinfo2) = !mc in
	let new_text =
	  match text with
	    Ast.BEFORE(bef) ->
	      Ast.BEFOREAFTER([Ast.Token "{"]::bef,[[Ast.Token "}"]])
	  | Ast.AFTER(aft) ->
	      Ast.BEFOREAFTER([[Ast.Token "{"]],aft@[[Ast.Token "}"]])
	  | Ast.BEFOREAFTER(bef,aft) ->
	      Ast.BEFOREAFTER([Ast.Token "{"]::bef,aft@[[Ast.Token "}"]])
	  | Ast.NOTHING ->
	      Ast.BEFOREAFTER([[Ast.Token "{"]],[[Ast.Token "}"]]) in
	Ast0.MIXED(ref(new_text,tinfo1,tinfo2))
    | _ -> failwith "unexpected plus code" in
  Ast0.set_mcodekind s new_mcodekind;
  s

let rec process dots_before dots_after =
  let donothing r k e = k e in
  let mcode x = x in

  let is_dots x =
    match Ast0.unwrap x with
      Ast0.Dots(_,_) | Ast0.Circles(_,_) | Ast0.Stars(_,_) -> true
    | _ -> false in

  let rec do_statement_dots dots_before dots_after = function
      [] -> []
    | dots::rest when is_dots dots ->
	dots::(do_statement_dots true dots_after rest)
    | [x] -> [(process dots_before dots_after).V0.rebuilder_statement x]
    | x::(dots::_ as rest) when is_dots dots ->
	((process dots_before true).V0.rebuilder_statement x)::
	do_statement_dots false dots_after rest
    | x::rest ->
	((process dots_before false).V0.rebuilder_statement x)::
	do_statement_dots false dots_after rest in

  let statement_dots r k d =
    Ast0.rewrap d
      (match Ast0.unwrap d with
	Ast0.DOTS(l) ->
	  Ast0.DOTS(do_statement_dots dots_before dots_after l)
      | Ast0.CIRCLES(l) ->
	  Ast0.CIRCLES(do_statement_dots dots_before dots_after l)
      | Ast0.STARS(l) ->
	  Ast0.STARS(do_statement_dots dots_before dots_after l)) in

  let get_option f = function
      Some x -> Some (f x)
    | None -> None in

  let statement r k s =
    match Ast0.unwrap s with
      Ast0.Nest(starter,stmt_dots,ender,whencode) ->
	Ast0.rewrap s
	  (Ast0.Nest(mcode starter,
		     (process true true).V0.rebuilder_statement_dots stmt_dots,
		     mcode ender,
		     get_option r.V0.rebuilder_statement_dots whencode))
    | Ast0.Disj(_,_,_,_) -> k s
    | Ast0.Exp(_) -> k s
    | Ast0.Dots(_,_) | Ast0.Circles(_,_) | Ast0.Stars(_,_) -> k s
    | Ast0.OptStm(_) | Ast0.UniqueStm(_) | Ast0.MultiStm(_) -> k s
    | _ ->
	let res = k s in
	if dots_before && dots_after && adding_something s
	then Ast0.set_dots_bef_aft res (Ast0.BetweenDots(add_braces res))
	else res in

  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing statement_dots
    donothing donothing donothing donothing donothing statement donothing

let single_statement l =
  List.map (process true true).V0.rebuilder_top_level l
