(* true = don't see all matched nodes, only modified ones *)
let onlyModif = ref true(*false*)

module Ast = Ast_cocci
module V = Visitor_ast
module CTL = Ast_ctl

let mcode r (_,_,kind) =
  match kind with
    Ast.MINUS(_) -> true
  | Ast.PLUS -> failwith "not possible"
  | Ast.CONTEXT(info) -> not (info = Ast.NOTHING)

let contains_modif x used_after =
  if List.exists (function x -> List.mem x used_after) (Ast.get_fvs x)
  then true
  else
    if !onlyModif
    then
      let bind x y = x or y in
      let option_default = false in
      let do_nothing r k e = k e in
      let rule_elem r k re =
	let res = k re in
	match Ast.unwrap re with
	  Ast.FunHeader(bef,_,stg,ty,name,lp,params,rp) ->
	    bind (mcode r ((),(),bef)) res
	| Ast.Decl(bef,decl) ->
	    bind (mcode r ((),(),bef)) res
	| _ -> res in
      let recursor =
	V.combiner bind option_default
	  mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
	  do_nothing do_nothing do_nothing
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

let extend data = function
    Opt x -> Opt x
  | Req x -> Req (data::x)

(* --------------------------------------------------------------------- *)
(* the main translation loop *)

let rec statement_list stmt_list used_after optional =
  match Ast.unwrap stmt_list with
    Ast.DOTS(x) | Ast.CIRCLES(x) | Ast.STARS(x) ->
      List.fold_left
	(function prev ->
	  function cur ->
	    lub (statement cur used_after optional, prev))
	(Opt []) x

and statement stmt used_after optional =
  match Ast.unwrap stmt with
    Ast.Atomic(ast) ->
      if contains_modif ast used_after then optional [ast] else Opt []

  | Ast.Seq(lbrace,decls,dots,body,rbrace) ->
      let body_info =
	lub (statement_list decls used_after optional,
	     statement_list body used_after optional) in
      if contains_modif lbrace used_after or contains_modif rbrace used_after
      then
	match body_info with
	  Req(elems) -> body_info (* don't bother adding braces *)
	| Opt(elems) -> lub (optional [lbrace;rbrace], body_info)
      else body_info

  | Ast.IfThen(header,branch,aft)
  | Ast.While(header,branch,aft) | Ast.For(header,branch,aft) ->
      if contains_modif header used_after or mcode () ((),(),aft)
      then optional [header]
      else extend header (statement branch used_after optional)

  | Ast.IfThenElse(ifheader,branch1,els,branch2,aft) ->
      if contains_modif ifheader used_after or mcode () ((),(),aft)
      then optional [ifheader]
      else
	extend ifheader
	  (lub(statement branch1 used_after optional,
	       statement branch2 used_after optional))

  | Ast.Disj(stmt_dots_list) ->
      List.fold_left
	(function prev ->
	  function cur ->
	    lub (statement_list cur used_after (function x -> Opt x), prev))
	(Opt []) stmt_dots_list

  | Ast.Nest(stmt_dots,whencode,t) ->
      (match Ast.unwrap stmt_dots with
	Ast.DOTS([l]) ->
	  (match Ast.unwrap l with
	    Ast.MultiStm(stm) ->
	      statement stm used_after optional
	  | _ -> statement_list stmt_dots used_after (function x -> Opt x))
      | _ -> statement_list stmt_dots used_after (function x -> Opt x))

  | Ast.Dots((_,i,d),whencodes,t) -> Opt []

  | Ast.FunDecl(header,lbrace,decls,dots,body,rbrace) ->
      let body_info =
	extend header (* only extends if the rest is required *)
	  (lub (statement_list decls used_after optional,
		statement_list body used_after optional)) in
      if contains_modif header used_after or
	contains_modif lbrace used_after or contains_modif rbrace used_after
      then
	match body_info with
	  Req(elems) -> body_info (* don't bother adding braces *)
	| Opt(elems) -> lub (optional [header], body_info)
      else body_info

  | Ast.OptStm(stm) -> statement stm used_after (function x -> Opt x)

  | Ast.UniqueStm(stm) | Ast.MultiStm(stm) -> statement stm used_after optional

  | _ -> failwith "not supported"

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let top_level ua t =
  match Ast.unwrap t with
    Ast.FILEINFO(old_file,new_file) -> failwith "not supported fileinfo"
  | Ast.DECL(stmt) ->
      statement stmt ua (function x -> Req x)
  | Ast.CODE(stmt_dots) ->
      statement_list stmt_dots ua (function x -> Req x)
  | Ast.ERRORWORDS(exps) -> failwith "not supported errorwords"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let debug = false

let asttomember l used_after =
  List.map
    (function
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
	  ([],List.map (function x -> (Lib_engine.Match(x),CTL.Control)) x))
  (List.map2 top_level used_after l)
