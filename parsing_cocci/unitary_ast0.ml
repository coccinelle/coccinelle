(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* find unitary metavariables *)
module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types
module TC = Type_cocci

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
    | Ast0.MetaLocalFunc(name,_,_) -> checker name
    | Ast0.DisjId(starter,id_list,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_ident id_list)
    | _ -> k i in

  let rec type_collect res = function
      TC.ConstVol(_,ty) | TC.Pointer(ty) | TC.FunctionPointer(ty)
    | TC.Array(ty) -> type_collect res ty
    | TC.EnumName(TC.MV(tyname,_,_)) ->
	bind [tyname] res
    | TC.StructUnionName(_,TC.MV(tyname,_,_)) ->
	bind [tyname] res
    | TC.MetaType(tyname,_,_) ->
	bind [tyname] res
    | TC.Decimal(e1,e2) ->
	let e2mv = function TC.MV(mv,_,_) -> [mv] | _ -> [] in
	bind (e2mv e1) (e2mv e2)
    | TC.SignedT(_,Some ty) -> type_collect res ty
    | ty -> res in

  let constraints_collect r res = function
      Ast0.NotExpCstrt(el) ->
	List.fold_left bind res
	  (List.map r.VT0.combiner_rec_expression el)
    | Ast0.SubExpCstrt(names) -> bind names res
    | _ -> res in

  let expression r k e =
    match Ast0.unwrap e with
      Ast0.MetaErr(name,constraints,_) ->
	let constraints =
	  constraints_collect r option_default constraints in
	bind (checker name) constraints
    | Ast0.MetaExpr(name,constraints,type_list,_,_) ->
	let types =
	  match type_list with
	    Some type_list ->
	      List.fold_left type_collect option_default type_list
	  | None -> option_default in
	let constraints =
	  constraints_collect r types constraints in
	bind (checker name) constraints
    | Ast0.MetaExprList(name,_,_) -> checker name
    | Ast0.DisjExpr(starter,expr_list,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_expression expr_list)
    | Ast0.ConjExpr(starter,expr_list,mids,ender) ->
	List.fold_left
	  (fun prev cur -> bind (r.VT0.combiner_rec_expression  cur) prev)
	  option_default expr_list
    | _ -> k e in

  let typeC r k t =
    match Ast0.unwrap t with
      Ast0.MetaType(name,_) -> checker name
    | Ast0.DisjType(starter,types,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_typeC types)
    | _ -> k t in

  let parameter r k p =
    match Ast0.unwrap p with
      Ast0.MetaParam(name,_) | Ast0.MetaParamList(name,_,_) -> checker name
    | _ -> k p in

  let declaration r k d =
    match Ast0.unwrap d with
      Ast0.MetaDecl(name,_) | Ast0.MetaField(name,_)
    | Ast0.MetaFieldList(name,_,_) -> checker name
    | Ast0.DisjDecl(starter,decls,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_declaration decls)
    | _ -> k d in

  let case_line r k c =
    match Ast0.unwrap c with
      Ast0.DisjCase(starter,case_lines,mids,ender) ->
	detect_unitary_frees(List.map r.VT0.combiner_rec_case_line case_lines)
    | _ -> k c in

  let statement r k s =
    match Ast0.unwrap s with
      Ast0.MetaStmt(name,_) | Ast0.MetaStmtList(name,_,_) -> checker name
    | Ast0.Disj(starter,stmt_list,mids,ender) ->
	detect_unitary_frees
	  (List.map r.VT0.combiner_rec_statement_dots stmt_list)
    | Ast0.Conj(starter,stmt_list,mids,ender) ->
	List.fold_left
	  (fun prev cur -> bind (r.VT0.combiner_rec_statement_dots  cur) prev)
	  option_default stmt_list
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

  let res = V0.combiner bind option_default
      {V0.combiner_functions with
	VT0.combiner_identfn = ident;
	VT0.combiner_exprfn = expression;
	VT0.combiner_tyfn = typeC;
	VT0.combiner_paramfn = parameter;
	VT0.combiner_declfn = declaration;
	VT0.combiner_stmtfn = statement;
	VT0.combiner_casefn = case_line} in

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

  let ident r k i =
    match Ast0.unwrap i with
      Ast0.MetaId(name,constraints,seed,_) ->
	Ast0.rewrap i (Ast0.MetaId(name,constraints,seed,is_unitary name))
    | Ast0.MetaFunc(name,constraints,_) ->
	Ast0.rewrap i (Ast0.MetaFunc(name,constraints,is_unitary name))
    | Ast0.MetaLocalFunc(name,constraints,_) ->
	Ast0.rewrap i (Ast0.MetaLocalFunc(name,constraints,is_unitary name))
    | _ -> k i in

  let expression r k e =
    match Ast0.unwrap e with
      Ast0.MetaErr(name,constraints,_) ->
	Ast0.rewrap e (Ast0.MetaErr(name,constraints,is_unitary name))
    | Ast0.MetaExpr(name,constraints,ty,form,_) ->
	Ast0.rewrap e (Ast0.MetaExpr(name,constraints,ty,form,is_unitary name))
    | Ast0.MetaExprList(name,lenname,_) ->
	Ast0.rewrap e (Ast0.MetaExprList(name,lenname,is_unitary name))
    | _ -> k e in

  let typeC r k t =
    match Ast0.unwrap t with
      Ast0.MetaType(name,_) ->
	Ast0.rewrap t (Ast0.MetaType(name,is_unitary name))
    | _ -> k t in

  let parameter r k p =
    match Ast0.unwrap p with
      Ast0.MetaParam(name,_) ->
	Ast0.rewrap p (Ast0.MetaParam(name,is_unitary name))
    | Ast0.MetaParamList(name,lenname,_) ->
	Ast0.rewrap p (Ast0.MetaParamList(name,lenname,is_unitary name))
    | _ -> k p in

  let statement r k s =
    match Ast0.unwrap s with
      Ast0.MetaStmt(name,_) ->
	Ast0.rewrap s (Ast0.MetaStmt(name,is_unitary name))
    | Ast0.MetaStmtList(name,lenname,_) ->
	Ast0.rewrap s (Ast0.MetaStmtList(name,lenname,is_unitary name))
    | _ -> k s in

  let res = V0.rebuilder
      {V0.rebuilder_functions with
	VT0.rebuilder_identfn = ident;
	VT0.rebuilder_exprfn = expression;
	VT0.rebuilder_tyfn = typeC;
	VT0.rebuilder_paramfn = parameter;
	VT0.rebuilder_stmtfn = statement} in

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
        Ast0.ScriptRule (_,_,_,_,_,_)
      | Ast0.InitialScriptRule (_,_,_,_,_)
      | Ast0.FinalScriptRule (_,_,_,_,_) ->
          let (x,rules) = loop rules in
          (x, r::rules)
      | Ast0.CocciRule ((minus,metavars,chosen_isos),((plus,_) as plusz),rt) ->
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
		((rebuilt, metavars, chosen_isos),plusz,rt))::rest) in
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
