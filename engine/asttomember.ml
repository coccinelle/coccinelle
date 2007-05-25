(* on the first pass, onlyModif is true, so we don't see all matched nodes,
only modified ones *)

module Ast = Ast_cocci
module V = Visitor_ast
module CTL = Ast_ctl

let mcode r (_,_,kind) =
  match kind with
    Ast.MINUS(_,_) -> true
  | Ast.PLUS -> failwith "not possible"
  | Ast.CONTEXT(_,info) -> not (info = Ast.NOTHING)

let contains_modif onlyModif x used_after =
  if List.exists (function x -> List.mem x used_after) (Ast.get_fvs x)
  then true
  else
    if onlyModif
    then
      let bind x y = x or y in
      let option_default = false in
      let do_nothing r k e = k e in
      let rule_elem r k re =
	let res = k re in
	match Ast.unwrap re with
	  Ast.FunHeader(bef,_,fninfo,name,lp,params,rp) ->
	    bind (mcode r ((),(),bef)) res
	| Ast.Decl(bef,_,decl) ->
	    bind (mcode r ((),(),bef)) res
	| _ -> res in
      let recursor =
	V.combiner bind option_default
	  mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
	  mcode
	  do_nothing do_nothing do_nothing do_nothing
	  do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
	  do_nothing rule_elem do_nothing do_nothing do_nothing do_nothing in
      recursor.V.combiner_rule_elem x
    else true

(* --------------------------------------------------------------------- *)
(* reqopt type *)

(* argument of Req is never empty *)
type reqopt = Req of Ast.rule_elem list | Opt of Ast.rule_elem list

let lub = function
    (Opt x1,Opt x2) -> Opt (x1@x2)
  | (Opt x1,Req x2) -> Req x2
  | (Req x1,Opt x2) -> Req x1
  | (Req x1,Req x2) -> Req (x1@x2)

let extend optional data = function
    Opt [] -> optional [data]
  | Opt x -> Opt x
  | Req x -> Req (data::x)

(* --------------------------------------------------------------------- *)
(* the main translation loop *)

let rec statement_list onlyModif tail stmt_list used_after optional =
  match Ast.unwrap stmt_list with
    Ast.DOTS(x) | Ast.CIRCLES(x) | Ast.STARS(x) ->
      (match List.rev x with
	[] -> Opt []
      |	last::rest ->
	  List.fold_right
	    (function cur ->
	      function rest ->
		lub (statement onlyModif false cur used_after optional, rest))
	    rest (statement onlyModif tail last used_after optional))

and statement onlyModif tail stmt used_after optional =
  match Ast.unwrap stmt with
    Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	(* modifications on return are managed in some other way *)
	Ast.Return(_,_) | Ast.ReturnExpr(_,_,_) when tail -> optional []
      |	_ ->
	  if contains_modif onlyModif ast used_after
	  then optional [ast]
	  else Opt [])
  | Ast.Seq(lbrace,decls,dots,body,rbrace) ->
      let body_info =
	lub (statement_list onlyModif false decls used_after optional,
	     statement_list onlyModif tail body used_after optional) in
      if contains_modif onlyModif lbrace used_after or contains_modif onlyModif rbrace used_after
      then
	match body_info with
	  Req(elems) -> body_info (* don't bother adding braces *)
	| Opt(elems) -> lub (optional [lbrace;rbrace], body_info)
      else body_info

  | Ast.IfThen(header,branch,(_,_,_,aft))
  | Ast.While(header,branch,(_,_,_,aft))
  | Ast.For(header,branch,(_,_,_,aft)) ->
      if contains_modif onlyModif header used_after or mcode () ((),(),aft)
      then optional [header]
      else extend optional header (statement onlyModif tail branch used_after optional)

  | Ast.Switch(header,lb,cases,rb) ->
      let body_info = case_lines  onlyModif tail cases used_after optional in
      if contains_modif onlyModif header used_after or
	contains_modif onlyModif lb used_after or
	contains_modif onlyModif rb used_after
      then
	match body_info with
	  Req(elems) -> body_info (* don't bother adding braces *)
	| Opt(elems) -> lub (optional [header;lb;rb], body_info)
      else body_info

  | Ast.IfThenElse(ifheader,branch1,els,branch2,(_,_,_,aft)) ->
      if contains_modif onlyModif ifheader used_after or mcode () ((),(),aft)
      then optional [ifheader]
      else
	extend optional ifheader
	  (lub(statement onlyModif tail branch1 used_after optional,
	       statement onlyModif tail branch2 used_after optional))

  | Ast.Disj(stmt_dots_list) ->
      List.fold_left
	(function prev ->
	  function cur ->
	    lub (statement_list onlyModif tail cur used_after (function x -> Opt x),
		 prev))
	(Opt []) stmt_dots_list

  | Ast.Nest(stmt_dots,whencode,t) ->
      (match Ast.unwrap stmt_dots with
	Ast.DOTS([l]) ->
	  (match Ast.unwrap l with
	    Ast.MultiStm(stm) ->
	      statement onlyModif tail stm used_after optional
	  | _ ->
	      statement_list onlyModif tail stmt_dots used_after (function x -> Opt x))
      | _ -> statement_list onlyModif tail stmt_dots used_after (function x -> Opt x))

  | Ast.Dots((_,i,d),whencodes,t) -> Opt []

  | Ast.FunDecl(header,lbrace,decls,dots,body,rbrace) ->
      let body_info =
	extend optional header (* only extends if the rest is required *)
	  (lub (statement_list onlyModif false decls used_after optional,
		statement_list onlyModif true body used_after optional)) in
      if contains_modif onlyModif header used_after or
	contains_modif onlyModif lbrace used_after or
	contains_modif onlyModif rbrace used_after
      then
	match body_info with
	  Req(elems) -> body_info (* don't bother adding braces *)
	| Opt(elems) -> lub (optional [header], body_info)
      else body_info

  | Ast.OptStm(stm) -> statement onlyModif tail stm used_after (function x -> Opt x)

  | Ast.UniqueStm(stm) | Ast.MultiStm(stm) ->
      statement onlyModif tail stm used_after optional

  | _ -> failwith "not supported"

and case_lines onlyModif tail cases used_after optional =
  match cases with
    [] -> Opt []
  | last::rest ->
      List.fold_right
	(function cur ->
	  function rest ->
	    lub (case_line onlyModif false cur used_after optional, rest))
	rest (case_line onlyModif tail last used_after optional)

and case_line onlyModif tail case used_after optional =
  match Ast.unwrap case with
    Ast.CaseLine(header,code) ->
      if contains_modif onlyModif header used_after
      then optional [header]
      else
	extend optional header (statement_list onlyModif tail code used_after optional)
  | Ast.OptCase(case) -> failwith "not supported"

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let top_level onlyModif ua t =
  match Ast.unwrap t with
    Ast.FILEINFO(old_file,new_file) -> failwith "not supported fileinfo"
  | Ast.DECL(stmt) ->
      statement onlyModif false stmt ua (function x -> Req x)
  | Ast.CODE(stmt_dots) ->
      statement_list onlyModif false stmt_dots ua (function x -> Req x)
  | Ast.ERRORWORDS(exps) -> failwith "not supported errorwords"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let debug = false

(* if we end up with nothing, we assume that this rule is only here because
someone depends on it, and thus we try again with onlyModif as false.
Alternatively, we could check that this rule is mentioned in some
dependency, but that would be a little mroe work, and doesn't seem
worthwhile. *)
let asttomember (_,_,l) used_after =
  let process_one = function
	Req x ->
	  if debug
	  then
	    List.iter
	      (function x ->
		Printf.printf "required %s\n"
		  (Pretty_print_cocci.rule_elem_to_string x))
	      x;
	  (List.map (function x -> (Lib_engine.Match(x),CTL.Control)) x,[])
      |	Opt x ->
	  if debug
	  then
	    List.iter
	      (function x ->
		Printf.printf "optional %s\n"
		  (Pretty_print_cocci.rule_elem_to_string x))
	      x;
	  ([],List.map (function x -> (Lib_engine.Match(x),CTL.Control)) x) in
  List.map2
    (function min -> function max ->
      match min with
	Req [] | Opt [] -> process_one max
      |	_ -> process_one min)
    (List.map2 (top_level false) used_after l)
    (List.map2 (top_level true) used_after l)
