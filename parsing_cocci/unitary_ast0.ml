(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* find unitary metavariables *)
module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

let set_minus s minus = List.filter (function n -> not (List.mem n minus)) s

let rec nub = function
    [] -> []
  | (x::xs) when (List.mem x xs) -> nub xs
  | (x::xs) -> x::(nub xs)

(* ----------------------------------------------------------------------- *)
(* Find the variables that occur free and occur free in a unitary way *)

(* take everything *)
let minus_checker name = let id = Ast0.unwrap_mcode name in [id]

(* take only what is in the plus code *)
let plus_checker (nm,_,_,mc,_,_) =
  match mc with Ast0.PLUS _ -> [nm] | _ -> []

let get_free checker t =
  let bind x y = x @ y in
  let option_default = [] in
  let donothing r k e = k e in

  (* considers a single list *)
  let collect_unitary_nonunitary free_usage =
    let free_usage = List.sort compare free_usage in
    let rec loop1 todrop = function
	[] -> []
      | (x::xs) as all -> if x = todrop then loop1 todrop xs else all in
    let rec loop2 = function
	[] -> ([],[])
      | [x] -> ([x],[])
      | x::y::xs ->
	  if x = y
	  then
	    let (unitary,non_unitary) = loop2(loop1 x xs) in
	    (unitary,x::non_unitary)
	  else
	    let (unitary,non_unitary) = loop2 (y::xs) in
	    (x::unitary,non_unitary) in
    loop2 free_usage in

  (* considers a list of lists *)
  let detect_unitary_frees l =
    let (unitary,nonunitary) =
      List.split (List.map collect_unitary_nonunitary l) in
    let unitary = nub (List.concat unitary) in
    let nonunitary = nub (List.concat nonunitary) in
    let unitary =
      List.filter (function x -> not (List.mem x nonunitary)) unitary in
    unitary@nonunitary@nonunitary in

  let whencode afn bfn expression = function
      Ast0.WhenNot(_,_,a) -> afn a
    | Ast0.WhenAlways(_,_,b) -> bfn b
    | Ast0.WhenModifier(_,_) -> option_default
    | Ast0.WhenNotTrue(_,_,a) -> expression a
    | Ast0.WhenNotFalse(_,_,a) -> expression a in

  let ident r k i =
    match Ast0.unwrap i with
      Ast0.MetaId(name,_,_,_) | Ast0.MetaFunc(name,_,_)
    | Ast0.MetaLocalFunc(name,_,_) -> bind (k i) (checker name)
    | Ast0.DisjId(starter,id_list,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_ident id_list)
    | _ -> k i in

  let type_collect res ty = bind res (Ast0.meta_names_of_typeC ty) in

  let mcode mc =
    List.fold_left
      (fun accu e ->
	match e with
	  Ast0.MetaPosTag(Ast0.MetaPos(name,constraints,_)) ->
	    Ast.cstr_fold
	      { Ast.empty_cstr_transformer with
		Ast.cstr_script =
		Some (fun (_,(name,lang,params,_pos,body)) accu ->
		  (* It seems that position variables are not relevant
		     for unitaryness, so drop them *)
		  bind (List.map fst
		     (List.filter
			(function
			    (_,Ast.MetaPosDecl _) -> false
			  | _ -> true)
			params)) accu) } constraints accu
	| Ast0.MetaPosTag(Ast0.MetaCom(name,constraints)) ->
	    bind (checker name)
	      (Ast.cstr_fold
		 { Ast.empty_cstr_transformer with
		   Ast.cstr_script =
		   Some (fun (_,(name,lang,params,_pos,body)) accu ->
		  (* It seems that position variables are not relevant
		     for unitaryness, so drop them *)
		     bind (List.map fst
			     (List.filter
				(function
				    (_,Ast.MetaPosDecl _) -> false
				  | _ -> true)
				params)) accu) } constraints accu)
	| _ -> accu)
      option_default (Ast0.get_pos mc) in

  let constraints_collect r res c =
    let cstr_expr =
      Some (fun e res -> bind res (r.VT0.combiner_rec_expression e)) in
    let cstr_meta_name = Some (fun mn res -> bind [mn] res) in
    let transformer =
      { Ast.empty_cstr_transformer with Ast.cstr_expr; Ast.cstr_meta_name } in
    Ast.cstr_fold transformer c res in

  let expression r k e =
    match Ast0.unwrap e with
      Ast0.MetaErr(name,constraints,_) ->
	let constraints =
	  constraints_collect r option_default constraints in
	bind (k e) (bind (checker name) constraints)
    | Ast0.MetaExpr(name,constraints,type_list,_,_,_bitfield) ->
	let types =
	  match type_list with
	    Some type_list ->
	      List.fold_left type_collect option_default type_list
	  | None -> option_default in
	let constraints =
	  constraints_collect r types constraints in
	bind (k e) (bind (checker name) constraints)
    | Ast0.MetaExprList(name,_,_,_) -> bind (k e) (checker name)
    | Ast0.DisjExpr(starter,expr_list,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_expression expr_list)
    | _ -> k e in
  
  let typeC r k t =
    match Ast0.unwrap t with
      Ast0.MetaType(name,_,_) -> bind (k t) (checker name)
    | Ast0.DisjType(starter,types,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_typeC types)
    | _ -> k t in

  let parameter r k p =
    match Ast0.unwrap p with
      Ast0.MetaParam(name,_,_) | Ast0.MetaParamList(name,_,_,_) ->
	bind (k p) (checker name)
    | _ -> option_default in

  let declaration r k d =
    match Ast0.unwrap d with
      Ast0.MetaDecl(name,_,_) -> bind (k d) (checker name)
    | Ast0.DisjDecl(starter,decls,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_declaration decls)
    | _ -> k d in

  let field r k d =
    match Ast0.unwrap d with
      Ast0.MetaField(name,_,_)
    | Ast0.MetaFieldList(name,_,_,_) -> bind (k d) (checker name)
    | Ast0.DisjField(starter,decls,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_field decls)
    | _ -> k d in

  let case_line r k c =
    match Ast0.unwrap c with
      Ast0.DisjCase(starter,case_lines,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_case_line case_lines)
    | _ -> k c in

  let attr_arg r k a =
    match Ast0.unwrap a with
      Ast0.MetaAttr(name,_,_) ->
	bind (k a) (checker name)
    | _ -> option_default in

  let statement r k s =
   match Ast0.unwrap s with
     Ast0.MetaStmt(name,_,_) | Ast0.MetaStmtList(name,_,_,_) ->
       bind (k s) (checker name)
   | Ast0.Disj(starter,stmt_list,mids,ender) ->
	  detect_unitary_frees
	    (List.map r.VT0.combiner_rec_statement_dots stmt_list)
   | Ast0.Nest(starter,stmt_dots,ender,whn,multi) ->
       bind (r.VT0.combiner_rec_statement_dots stmt_dots)
	 (detect_unitary_frees
	    (List.map
	       (whencode
		  r.VT0.combiner_rec_statement_dots
		  r.VT0.combiner_rec_statement
		  r.VT0.combiner_rec_expression)
	       whn))
   | Ast0.Dots(d,whn) ->
       detect_unitary_frees
	 (List.map
	    (whencode
	       r.VT0.combiner_rec_statement_dots r.VT0.combiner_rec_statement
	       r.VT0.combiner_rec_expression)
	    whn)
   | _ -> k s in

  let res =
    V0.flat_combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing
      ident expression donothing donothing typeC donothing parameter
      declaration field donothing statement donothing case_line donothing
      donothing attr_arg donothing in

  collect_unitary_nonunitary
    (List.concat (List.map res.VT0.combiner_rec_top_level t))

(* ----------------------------------------------------------------------- *)
(* update the variables that are unitary *)

let update_unitary unitary =
  let is_unitary name =
    match (List.mem (Ast0.unwrap_mcode name) unitary,
	   !Flag.sgrep_mode2, Ast0.get_mcode_mcodekind name) with
      (true,true,_) | (true,_,Ast0.CONTEXT(_)) -> Ast0.PureContext
    | (true,_,_) -> Ast0.Pure
    | (false,true,_) | (false,_,Ast0.CONTEXT(_)) -> Ast0.Context
    | (false,_,_) -> Ast0.Impure in

  let mcode mc =
    List.iter
      (function
	  Ast0.MetaPosTag(Ast0.MetaCom(name,constraints)) ->
	    if not (List.mem (Ast0.unwrap_mcode name) unitary)
	    then
	      failwith
		(Printf.sprintf "line %d: comment variable %s must be used only once"
		   (Ast0.get_mcode_line name) (snd (Ast0.unwrap_mcode name)))
	| _ -> ())
      (Ast0.get_pos mc);
    mc in

  let ident r k i =
    let i = k i in
    match Ast0.unwrap i with
      Ast0.MetaId(name,constraints,seed,_) ->
	Ast0.rewrap i (Ast0.MetaId(name,constraints,seed,is_unitary name))
    | Ast0.MetaFunc(name,constraints,_) ->
	Ast0.rewrap i (Ast0.MetaFunc(name,constraints,is_unitary name))
    | Ast0.MetaLocalFunc(name,constraints,_) ->
	Ast0.rewrap i (Ast0.MetaLocalFunc(name,constraints,is_unitary name))
    | _ -> i in

  let expression r k e =
    let e = k e in
    match Ast0.unwrap e with
      Ast0.MetaErr(name,constraints,_) ->
	Ast0.rewrap e (Ast0.MetaErr(name,constraints,is_unitary name))
    | Ast0.MetaExpr(name,constraints,ty,form,_,bitfield) ->
	Ast0.rewrap e
	  (Ast0.MetaExpr(name,constraints,ty,form,is_unitary name,bitfield))
    | Ast0.MetaExprList(name,lenname,cstr,_) ->
	Ast0.rewrap e (Ast0.MetaExprList(name,lenname,cstr,is_unitary name))
    | _ -> e in

  let typeC r k t =
    let t = k t in
    match Ast0.unwrap t with
      Ast0.MetaType(name,cstr,_) ->
	Ast0.rewrap t (Ast0.MetaType(name,cstr,is_unitary name))
    | _ -> t in

  let parameter r k p =
    let p = k p in
    match Ast0.unwrap p with
      Ast0.MetaParam(name,cstr,_) ->
	Ast0.rewrap p (Ast0.MetaParam(name,cstr,is_unitary name))
    | Ast0.MetaParamList(name,lenname,cstr,_) ->
	Ast0.rewrap p (Ast0.MetaParamList(name,lenname,cstr,is_unitary name))
    | _ -> p in

  let declaration r k d =
    let d = k d in
    match Ast0.unwrap d with
      Ast0.MetaDecl(name,cstr,_) ->
	Ast0.rewrap d (Ast0.MetaDecl(name,cstr,is_unitary name))
    | _ -> d in

  let statement r k s =
    let s = k s in
    match Ast0.unwrap s with
      Ast0.MetaStmt(name,cstr,_) ->
	Ast0.rewrap s (Ast0.MetaStmt(name,cstr,is_unitary name))
    | Ast0.MetaStmtList(name,lenname,cstr,_) ->
	Ast0.rewrap s (Ast0.MetaStmtList(name,lenname,cstr,is_unitary name))
    | _ -> s in

  let res = V0.rebuilder
      {V0.rebuilder_functions with
	VT0.rebuilder_meta_mcode = mcode;
	VT0.rebuilder_string_mcode = mcode;
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
	VT0.rebuilder_identfn = ident;
	VT0.rebuilder_exprfn = expression;
	VT0.rebuilder_tyfn = typeC;
	VT0.rebuilder_paramfn = parameter;
	VT0.rebuilder_stmtfn = statement;
	VT0.rebuilder_declfn = declaration} in

  List.map res.VT0.rebuilder_rec_top_level

(* ----------------------------------------------------------------------- *)

let rec split3 = function
    [] -> ([],[],[])
  | (a,b,c)::xs -> let (l1,l2,l3) = split3 xs in (a::l1,b::l2,c::l3)

let rec combine3 = function
    ([],[],[]) -> []
  | (a::l1,b::l2,c::l3) -> (a,b,c) :: combine3 (l1,l2,l3)
  | _ -> failwith "not possible"

(* ----------------------------------------------------------------------- *)
(* process all rules *)

let do_unitary rules =
  let rec loop = function
      [] -> ([],[])
    | (r::rules) ->
      match r with
        Ast0.ScriptRule (_,_,_,_,_,_,_)
      | Ast0.InitialScriptRule (_,_,_,_,_,_)
      | Ast0.FinalScriptRule (_,_,_,_,_,_) ->
          let (x,rules) = loop rules in
          (x, r::rules)
      | Ast0.CocciRule((minus,metavars,chosen_isos),((plus,_) as plusz),inh,rt) ->
          let mm1 = List.map Ast.get_meta_name metavars in
          let (used_after, rest) = loop rules in
          let (m_unitary, m_nonunitary) = get_free minus_checker minus in
          let (p_unitary, p_nonunitary) = get_free plus_checker plus in
          let p_free =
            if !Flag.sgrep_mode2 then []
            else p_unitary @ p_nonunitary in
          let (in_p, m_unitary) =
            List.partition (function x -> List.mem x p_free) m_unitary in
          let m_nonunitary = in_p @ m_nonunitary in
          let (m_unitary, not_local) =
            List.partition (function x -> List.mem x mm1) m_unitary in
          let m_unitary =
            List.filter (function x -> not (List.mem x used_after))
	      m_unitary in
          let rebuilt = update_unitary m_unitary minus in
          (set_minus (m_nonunitary @ used_after) mm1,
             (Ast0.CocciRule
		((rebuilt, metavars, chosen_isos),plusz,inh,rt))::rest) in
  let (_,rules) = loop rules in
  rules

(*
let do_unitary minus plus =
  let (minus,metavars,chosen_isos) = split3 minus in
  let (plus,_) = List.split plus in
  let rec loop = function
      ([],[],[]) -> ([],[])
    | (mm1::metavars,m1::minus,p1::plus) ->
	let mm1 = List.map Ast.get_meta_name mm1 in
	let (used_after,rest) = loop (metavars,minus,plus) in
	let (m_unitary,m_nonunitary) = get_free minus_checker m1 in
	let (p_unitary,p_nonunitary) = get_free plus_checker p1 in
	let p_free =
	  if !Flag.sgrep_mode2
	  then []
	  else p_unitary @ p_nonunitary in
	let (in_p,m_unitary) =
	  List.partition (function x -> List.mem x p_free) m_unitary in
	let m_nonunitary = in_p@m_nonunitary in
	let (m_unitary,not_local) =
	  List.partition (function x -> List.mem x mm1) m_unitary in
	let m_unitary =
	  List.filter (function x -> not(List.mem x used_after)) m_unitary in
	let rebuilt = update_unitary m_unitary m1 in
	(set_minus (m_nonunitary @ used_after) mm1,
	 rebuilt::rest)
    | _ -> failwith "not possible" in
  let (_,rules) = loop (metavars,minus,plus) in
  combine3 (rules,metavars,chosen_isos)
*)
