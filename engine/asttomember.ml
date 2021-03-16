(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* on the first pass, onlyModif is true, so we don't see all matched nodes,
only modified ones *)

module Ast = Ast_cocci
module V = Visitor_ast
module CTL = Ast_ctl

let mcode r (_,_,kind,_) =
  match kind with
    Ast.MINUS(_,_,_,_) -> true
  | Ast.PLUS _ -> failwith "mcode: PLUS not possible"
  | Ast.CONTEXT(_,info) -> not (info = Ast.NOTHING)

let no_mcode _ _ = false

let contains_modif used_after x =
  if List.exists (function x -> List.mem x used_after) (Ast.get_fvs x)
  then true
  else
    let bind x y = x || y in
    let option_default = false in
    let do_nothing r k e = k e in
    let annotated_decl_bef decl =
      match Ast.unwrap decl with
	Ast.DElem(bef,_,_) -> bef in
    let rule_elem r k re =
      let res = k re in
      match Ast.unwrap re with
	Ast.FunHeader(bef,_,fninfo,name,lp,params,va,rp) ->
	  bind (mcode r ((),(),bef,[])) res
      | Ast.Decl decl ->
	  bind (mcode r ((),(),annotated_decl_bef decl,[])) res
      | Ast.ForHeader(fr,lp,Ast.ForDecl decl,e2,sem2,e3,rp) ->
	  bind (mcode r ((),(),annotated_decl_bef decl,[])) res
      | _ -> res in
    let recursor =
      V.combiner bind option_default
	mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
	mcode mcode mcode
	do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
	do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
	do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
	do_nothing do_nothing do_nothing
	do_nothing do_nothing rule_elem do_nothing do_nothing do_nothing
	do_nothing do_nothing do_nothing in
    recursor.V.combiner_rule_elem x

(* contains an inherited metavariable or contains a constant *)
let contains_constant x =
  match Ast.get_inherited x with
    [] ->
      let bind x y = x || y in
      let option_default = false in
      let do_nothing r k e = k e in
      let mcode _ _ = false in
      let ident r k i =
	match Ast.unwrap i with
	  Ast.Id(name) -> true
	| _ -> k i in
      let expr r k e =
	match Ast.unwrap e with
	  Ast.Constant(const) -> true
	| _ -> k e in
      let recursor =
	V.combiner bind option_default
	  mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
	  mcode mcode mcode
	  do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
	  do_nothing
	  ident expr do_nothing do_nothing do_nothing do_nothing do_nothing
	  do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
	  do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
	  do_nothing do_nothing do_nothing do_nothing in
      recursor.V.combiner_rule_elem x
  | _ -> true

(* --------------------------------------------------------------------- *)

let print_info = function
    [] -> Printf.printf "no information\n"
  | l ->
      List.iter
	(function disj ->
	  Printf.printf "one set of required things %d:\n"
	    (List.length disj);
	  List.iter
	    (function (_,_,thing) ->
	      Printf.printf "%s\n"
		(Dumper.dump thing.Ast.positive_inherited_positions);
	      Printf.printf "%s\n"
		(Pretty_print_cocci.rule_elem_to_string thing))
	    disj)
	l

(* --------------------------------------------------------------------- *)

(* drop all distinguishing information from a term except inherited
   variables, which are used to improve efficiency of matching process *)
let strip x =
  let do_nothing r k e =
    let inh = Ast.get_inherited e in
    let inh_pos = Ast.get_inherited_pos e in
    Ast.make_inherited_term (Ast.unwrap (k e)) inh inh_pos in
  let do_absolutely_nothing r k e = k e in
  let mcode m = Ast.make_mcode(Ast.unwrap_mcode m) in
  let decl_or_field r k d =
    let res = do_nothing r k d in
    {res with Ast.safe_for_multi_decls = Ast.get_safe_decl d} in
  let annotated_decl no_mcode decl =
    match Ast.unwrap decl with
      Ast.DElem(bef,allminus,d) ->
	Ast.rewrap decl (Ast.DElem(no_mcode,allminus,d)) in
  let rule_elem r k re =
    let res = do_nothing r k re in
    let no_mcode = Ast.CONTEXT(Ast.NoPos,Ast.NOTHING) in
    match Ast.unwrap res with
      Ast.FunHeader(bef,b,fninfo,name,lp,params,va,rp) ->
	Ast.rewrap res
	  (Ast.FunHeader(no_mcode,b,fninfo,name,lp,params,va,rp))
    | Ast.Decl decl -> Ast.rewrap res (Ast.Decl(annotated_decl no_mcode decl))
    | Ast.ForHeader(fr,lp,Ast.ForDecl(decl),e2,sem2,e3,rp) ->
	Ast.rewrap res
	  (Ast.ForHeader(fr,lp,Ast.ForDecl(annotated_decl no_mcode decl),
			 e2,sem2,e3,rp))
    | _ -> res in
  let r =
    V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode mcode
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      decl_or_field do_absolutely_nothing decl_or_field do_absolutely_nothing
      do_nothing rule_elem
      do_nothing do_nothing do_nothing do_nothing do_nothing do_absolutely_nothing in
  r.V.rebuilder_rule_elem x

(* --------------------------------------------------------------------- *)

let disj l1 l2 = l1 l2

let conj xs ys =
  match (xs,ys) with
    ([],_) -> ys
  | (_,[]) -> xs
  | _ ->
      List.fold_left
	(function prev ->
	  function x ->
	    List.fold_left
	      (function prev ->
		function cur ->
		  let cur_res = (List.sort compare (Common.union_set x cur)) in
		  cur_res ::
		  (List.filter
		     (function x -> not (Common.include_set cur_res x))
		     prev))
	      prev ys)
	[] xs

let conj_wrapped x l = conj [List.map (function x -> (1,None,strip x)) x] l

(* --------------------------------------------------------------------- *)
(* the main translation loop *)

(* big disjunctions arising from isomorphisms may have some symbols or
   constants in common *)
(* to reduce cost, want at most one thing; take the longest *)
let find_commonalities res : Ast_cocci.rule_elem option =
  let get_symbols_and_constants x =
    let bind x y = Common.union_set x y in
    let one sz x = [(sz,strip(Ast.rewrap x (Ast.Exp x)))] in
    let option_default = [] in
    let do_nothing r k e = k e in
    let mcode _ _ = [] in
    let expression r k e =
      match Ast.unwrap e with
	Ast.Ident(id) ->
	  (match Ast.unwrap id with
	    Ast.Id(name) ->
	      let sz = String.length (Ast.unwrap_mcode name) in
	      bind (one sz e) (k e)
	  | _ -> k e)
      | Ast.Constant(const) ->
	  let sz =
	    match Ast.unwrap_mcode const with
	      Ast.String(s,sz) -> String.length s + 2
	    | Ast.Char(s,sz) -> String.length s + 2
	    | Ast.Int(s) -> String.length s
	    | Ast.Float(s) -> String.length s
	    | Ast.DecimalConst(s,_,_) -> String.length s in
	  bind (one sz e) (k e)
      | _ -> k e in
    let recursor =
      V.combiner bind option_default
	mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
	mcode mcode mcode
	do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
	do_nothing do_nothing
	expression do_nothing do_nothing do_nothing do_nothing
	do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
	do_nothing do_nothing do_nothing do_nothing
	do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
	do_nothing in
    recursor.V.combiner_rule_elem x in
  match res with
    [] -> failwith "bad disjunction"
  | x::xs ->
      let choices =
	let rec loop prev l =
	  if prev = []
	  then []
	  else
	    match l with
	      [] -> prev
	    | re::rest ->
		loop (Common.inter_set (get_symbols_and_constants re) prev)
		  rest in
	loop (get_symbols_and_constants x) xs in
      let choices = List.rev(List.sort compare choices) in
      match choices with
	[] -> None
      | x::xs -> Some(snd x) (* drop size *)

let rule_elem re =
  match Ast.unwrap re with
    Ast.DisjRuleElem(res) ->
      (* why was the following done?  ors have to be kept together for
	efficiency, so they are considered at once and not individually
        anded with everything else *)
      let re =
	let all_inhs = List.map Ast.get_inherited res in
	let inhs =
	  List.fold_left
	    (function prev ->
	      function inh ->
		Common.inter_set inh prev)
	    (List.hd all_inhs) (List.tl all_inhs) in
	let inhs_poss = Ast.get_inherited_pos re in (* already intersection *)
	Ast.make_inherited_term (Ast.unwrap re) inhs inhs_poss in
      [[(List.length res,find_commonalities res,strip re)]]
  | _ -> [[(1,None,strip re)]]

let conj_one testfn x l =
  if testfn x
  then conj (rule_elem x) l
  else l

let rec statement_list testfn mcode tail stmt_list : 'a list list =
  match List.rev (Ast.unwrap stmt_list) with
    [] -> []
  | last::rest ->
      List.fold_right
	(function cur ->
	  function rest ->
	    conj (statement testfn mcode false cur) rest)
	rest (statement testfn mcode tail last)

and statement testfn mcode tail stmt : 'a list list =
  match Ast.unwrap stmt with
    Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	(* modifications on return are managed in some other way *)
	Ast.Return(_,_) | Ast.ReturnExpr(_,_,_) when tail -> []
      |	_ -> if testfn ast then rule_elem ast else [])
  | Ast.Seq(lbrace,body,rbrace) ->
      let body_info = statement_list testfn mcode tail body in
      if testfn lbrace || testfn rbrace
      then conj_wrapped [lbrace;rbrace] body_info
      else body_info

  | Ast.IfThen(header,branch,(_,_,_,aft))
  | Ast.While(header,branch,(_,_,_,aft))
  | Ast.For(header,branch,(_,_,_,aft))
  | Ast.Iterator(header,branch,(_,_,_,aft)) ->
      if testfn header || mcode () ((),(),aft,[])
      then conj (rule_elem header) (statement testfn mcode tail branch)
      else statement testfn mcode tail branch

  | Ast.Switch(header,lb,decls,cases,rb) ->
      let body_info =
	conj
	  (statement_list testfn mcode false decls)
	  (case_lines testfn mcode tail cases) in
      if testfn header || testfn lb || testfn rb
      then conj (rule_elem header) body_info
      else body_info

  | Ast.IfThenElse(ifheader,branch1,els,branch2,(_,_,_,aft)) ->
      let branches =
	conj
	  (statement testfn mcode tail branch1)
	  (statement testfn mcode tail branch2) in
      if testfn ifheader || mcode () ((),(),aft,[])
      then conj (rule_elem ifheader) branches
      else branches

  | Ast.Disj(stmt_dots_list) ->
      let processed =
	List.map (statement_list testfn mcode tail) stmt_dots_list in
      (* if one branch gives no information, then we have to take anything *)
      if List.exists (function [] -> true | _ -> false) processed
      then []
      else Common.union_all processed

  | Ast.Conj(stmt_dots_list) ->
      let processed =
	List.map (statement_list testfn mcode tail) stmt_dots_list in
      Common.inter_all processed

  | Ast.Nest(starter,stmt_dots,ender,whencode,true,_,_) ->
      statement_list testfn mcode false stmt_dots

  | Ast.Nest(starter,stmt_dots,ender,whencode,false,_,_) -> []

  | Ast.Dots(_,whencodes,_,_) -> []

  | Ast.FunDecl(header,lbrace,body,rbrace,(_,_,_,aft)) ->
      let body_info = statement_list testfn mcode true body in
      if testfn header || testfn lbrace || testfn rbrace ||
	mcode () ((),(),aft,[])
      then conj (rule_elem header) body_info
      else body_info

  | Ast.Define(header,body) ->
      conj_one testfn header (statement_list testfn mcode tail body)

  | Ast.AsStmt(stm,asstm) ->
      conj
	(statement testfn mcode tail stm)
	(statement testfn mcode tail asstm)

  | Ast.OptStm(stm) -> []

  | _ -> failwith "not supported"

and case_lines testfn mcode tail cases =
  match cases with
    [] -> []
  | last::rest ->
      List.fold_right
	(function cur ->
	  function rest ->
	    conj (case_line testfn mcode false cur) rest)
	rest (case_line testfn mcode tail last)

and case_line testfn mcode tail case =
  match Ast.unwrap case with
    Ast.CaseLine(header,code) ->
      conj_one testfn header (statement_list testfn mcode tail code)

  | Ast.OptCase(case) -> []

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let top_level testfn mcode t : 'a list list =
  match Ast.unwrap t with
    Ast.FILEINFO(old_file,new_file) -> failwith "not supported fileinfo"
  | Ast.NONDECL(stmt) -> statement testfn mcode false stmt
  | Ast.CODE(stmt_dots) -> statement_list testfn mcode false stmt_dots
  | Ast.ERRORWORDS(exps) -> failwith "not supported errorwords"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let debug = false

(* if we end up with nothing, we assume that this rule is only here because
someone depends on it, and thus we try again with testfn as contains_modif.
Alternatively, we could check that this rule is mentioned in some
dependency, but that would be a little more work, and doesn't seem
worthwhile. *)

(* lists are sorted such that smaller DisjRuleElem are first, because they
are cheaper to test *)

let asttomemberz (_,_,l) used_after =
  let process_one
      (l : (int * Ast_cocci.rule_elem option * Ast_cocci.rule_elem) list list)
      =
    if debug
    then print_info l;
    List.map
      (function info ->
        let info =
          List.sort (function (n1,_,_) -> function (n2,_,_) -> compare n1 n2)
            info in
	let info =
	  let (with_pos,without_pos) =
	    (* put cases with inherited positions first *)
	    List.partition
	      (function (_,_,thing) ->
		not (thing.Ast.positive_inherited_positions = []))
	      info in
	  with_pos @ without_pos in
        List.map
	  (function (_,extra,x) ->
	    match extra with
	      None -> (Lib_engine.Match(x),CTL.Control)
	    | Some re -> (Lib_engine.Match(re),CTL.Control))
	  info)
      l in
  List.map2
    (function min -> function (max,big_max) ->
      match min with
        [] ->
          (match max() with
            [] -> process_one (big_max())
          | max -> process_one max)
      | _ -> process_one min)
    (List.map (top_level contains_constant no_mcode) l)
    (List.combine
       (List.map2
          (function x -> function ua -> function _ ->
            top_level (contains_modif ua) mcode x)
          l used_after)
       (List.map
          (function x -> function _ ->
            top_level (function _ -> true) no_mcode x)
          l))

let asttomember r used_after =
  match r with
    Ast.ScriptRule _ | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ -> []
  | Ast.CocciRule (a,b,c,_,_) -> asttomemberz (a,b,c) used_after
