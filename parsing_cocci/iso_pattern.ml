(* Potential problem: offset of mcode is not updated when an iso is
instantiated, implying that a term may end up with many mcodes with the
same offset.  On the other hand, at the moment offset only seems to be used
before this phase.  Furthermore add_dot_binding relies on the offset to
remain the same between matching an iso and instantiating it with bindings. *)

(* --------------------------------------------------------------------- *)
(* match a SmPL expression against a SmPL abstract syntax tree,
either - or + *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0

(* --------------------------------------------------------------------- *)

type isomorphism = Ast_cocci.metavar list * Ast0_cocci.anything list list

let strip_info =
  let mcode (term,_,_,_) = (term,Ast0.NONE,Ast0.default_info(),Ast0.PLUS) in
  let donothing r k e =
    let (term,info,index,mc,ty,dots) = k e in
    (term,Ast0.default_info(),ref 0,ref Ast0.PLUS,ref None,Ast0.NoDots) in
  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing

let anything_equal = function
    (Ast0.DotsExprTag(d1),Ast0.DotsExprTag(d2)) ->
      failwith "not a possible variable binding"
  | (Ast0.DotsInitTag(d1),Ast0.DotsInitTag(d2)) ->
      failwith "not a possible variable binding"
  | (Ast0.DotsParamTag(d1),Ast0.DotsParamTag(d2)) ->
      failwith "not a possible variable binding"
  | (Ast0.DotsStmtTag(d1),Ast0.DotsStmtTag(d2)) ->
      failwith "not a possible variable binding"
  | (Ast0.IdentTag(d1),Ast0.IdentTag(d2)) ->
      (strip_info.V0.rebuilder_ident d1) = (strip_info.V0.rebuilder_ident d2)
  | (Ast0.ExprTag(d1),Ast0.ExprTag(d2)) ->
      (strip_info.V0.rebuilder_expression d1) =
      (strip_info.V0.rebuilder_expression d2)
  | (Ast0.TypeCTag(d1),Ast0.TypeCTag(d2)) ->
      (strip_info.V0.rebuilder_typeC d1) =
      (strip_info.V0.rebuilder_typeC d2)
  | (Ast0.InitTag(d1),Ast0.InitTag(d2)) ->
      (strip_info.V0.rebuilder_initialiser d1) =
      (strip_info.V0.rebuilder_initialiser d2)
  | (Ast0.ParamTag(d1),Ast0.ParamTag(d2)) ->
      (strip_info.V0.rebuilder_parameter d1) =
      (strip_info.V0.rebuilder_parameter d2)
  | (Ast0.DeclTag(d1),Ast0.DeclTag(d2)) ->
      (strip_info.V0.rebuilder_declaration d1) =
      (strip_info.V0.rebuilder_declaration d2)
  | (Ast0.StmtTag(d1),Ast0.StmtTag(d2)) ->
      (strip_info.V0.rebuilder_statement d1) =
      (strip_info.V0.rebuilder_statement d2)
  | (Ast0.MetaTag(d1),Ast0.MetaTag(d2)) ->
      (strip_info.V0.rebuilder_meta d1) =
      (strip_info.V0.rebuilder_meta d2)
  | (Ast0.TopTag(d1),Ast0.TopTag(d2)) ->
      (strip_info.V0.rebuilder_top_level d1) =
      (strip_info.V0.rebuilder_top_level d2)
  | _ -> false

let term (var1,_,_,_) = var1
let dot_term  (var1,_,info,_) =
  var1 ^ (string_of_int info.Ast0.offset)

let add_binding var exp bindings =
  let var = term var in
  try
    let cur = List.assoc var bindings in
    if anything_equal(exp,cur) then Some bindings else None
  with Not_found -> Some ((var,exp)::bindings)

let add_dot_binding var exp bindings =
  let var = dot_term var in
  try
    let cur = List.assoc var bindings in
    if anything_equal(exp,cur) then Some bindings else None
  with Not_found -> Some ((var,exp)::bindings)

(* --------------------------------------------------------------------- *)

let init_env = []

let conjunct_bindings
    (m1 : 'binding -> 'binding option)
    (m2 : 'binding -> 'binding option)
    (binding : 'binding) : 'binding option =
  match m1 binding with None -> None | Some binding -> m2 binding

let mcode_equal (x,_,_,_) (y,_,_,_) = x = y

let return b binding = if b then Some binding else None

let match_option f t1 t2 =
  match (t1,t2) with
    (Some t1, Some t2) -> f t1 t2
  | (None, None) -> return true
  | _ -> return false

let bool_match_option f t1 t2 =
  match (t1,t2) with
    (Some t1, Some t2) -> f t1 t2
  | (None, None) -> true
  | _ -> false

let is_context e =
  match Ast0.get_mcodekind e with Ast0.CONTEXT(cell) -> true | _ -> false

let match_list matcher is_list_matcher do_list_match la lb =
  let rec loop = function
      ([],[]) -> return true
    | ([x],lb) when is_list_matcher x -> do_list_match x lb
    | (x::xs,y::ys) -> conjunct_bindings (matcher x y) (loop (xs,ys))
    | _ -> return false in
  loop (la,lb)

let match_maker context_required whencode_allowed =

  let match_dots matcher is_list_matcher do_list_match d1 d2 =
    match (Ast0.unwrap d1, Ast0.unwrap d2) with
      (Ast0.DOTS(la),Ast0.DOTS(lb))
    | (Ast0.CIRCLES(la),Ast0.CIRCLES(lb))
    | (Ast0.STARS(la),Ast0.STARS(lb)) ->
	match_list matcher is_list_matcher (do_list_match d2) la lb
    | _ -> return false in

  let is_elist_matcher el =
    match Ast0.unwrap el with Ast0.MetaExprList(_,_) -> true | _ -> false in

  let is_plist_matcher pl =
    match Ast0.unwrap pl with Ast0.MetaParamList(_,_) -> true | _ -> false in

  let is_slist_matcher pl =
    match Ast0.unwrap pl with Ast0.MetaStmtList(_,_) -> true | _ -> false in

  let no_list _ = false in

  let build_dots pattern data =
    match Ast0.unwrap pattern with
      Ast0.DOTS(_) -> Ast0.rewrap pattern (Ast0.DOTS(data))
    | Ast0.CIRCLES(_) -> Ast0.rewrap pattern (Ast0.CIRCLES(data))
    | Ast0.STARS(_) -> Ast0.rewrap pattern (Ast0.STARS(data)) in

  let pure_name = function
      Ast0.CONTEXT(mc) ->
	(match !mc with (Ast.NOTHING,_,_) -> true |_ -> false)
    | Ast0.MINUS(mc) ->
	(match !mc with ([],_) -> true | _ -> false)
    | _ -> false in

  let add_pure_list_binding name pure extract_other builder1 builder2 lst =
    if pure
    then
      (match lst with
	[x] ->
	  (match extract_other (Ast0.unwrap x) with
	    Some name1 ->
	      if pure_name (Ast0.get_mcodekind x) &&
		pure_name (Ast0.get_mcode_mcodekind name1)
	      then add_binding name (builder1 lst)
	      else return false
	  | None -> return false)
      | _ -> return false)
    else add_binding name (builder2 lst) in

  let add_pure_binding name pure extract_other builder x =
    if pure
    then
      (match extract_other (Ast0.unwrap x) with
	Some name1 ->
	  if pure_name (Ast0.get_mcodekind x) &&
	    pure_name (Ast0.get_mcode_mcodekind name1)
	  then add_binding name (builder x)
	  else return false
      | None -> return false)
    else add_binding name (builder x) in

  let do_elist_match builder el lst =
    match Ast0.unwrap el with
      Ast0.MetaExprList(name,pure) ->
	add_pure_list_binding name pure
	  (function Ast0.MetaExprList(name1,true) -> Some name1 | _ -> None)
	  (function lst -> Ast0.ExprTag(List.hd lst))
	  (function lst -> Ast0.DotsExprTag(build_dots builder lst))
	  lst
    | _ -> failwith "not possible" in

  let do_plist_match builder pl lst =
    match Ast0.unwrap pl with
      Ast0.MetaParamList(name,pure) ->
	add_pure_list_binding name pure
	  (function Ast0.MetaParamList(name1,true) -> Some name1 | _ -> None)
	  (function lst -> Ast0.ParamTag(List.hd lst))
	  (function lst -> Ast0.DotsParamTag(build_dots builder lst))
	  lst
    | _ -> failwith "not possible" in

  let do_slist_match builder sl lst =
    match Ast0.unwrap sl with
      Ast0.MetaStmtList(name,pure) ->
	add_pure_list_binding name pure
	  (function Ast0.MetaStmtList(name1,true) -> Some name1 | _ -> None)
	  (function lst -> Ast0.StmtTag(List.hd lst))
	  (function lst -> Ast0.DotsStmtTag(build_dots builder lst))
	  lst
    | _ -> failwith "not possible" in

  let do_nolist_match _ _ = failwith "not possible" in

  let rec match_ident pattern id =
    match Ast0.unwrap pattern with
      Ast0.MetaId(name,pure) ->
	add_pure_binding name pure
	  (function Ast0.MetaId(name1,true) -> Some name1 | _ -> None)
	  (function id -> Ast0.IdentTag id)
	  id
    | Ast0.MetaFunc(name,pure) -> failwith "metafunc not supported"
    | Ast0.MetaLocalFunc(name,pure) -> failwith "metalocalfunc not supported"
    | up ->
	if not(context_required) or is_context id
	then
	  match (up,Ast0.unwrap id) with
	    (Ast0.Id(namea),Ast0.Id(nameb)) -> return (mcode_equal namea nameb)
	  | (Ast0.OptIdent(ida),Ast0.OptIdent(idb))
	  | (Ast0.UniqueIdent(ida),Ast0.UniqueIdent(idb))
	  | (Ast0.MultiIdent(ida),Ast0.MultiIdent(idb)) -> match_ident ida idb
	  | (_,Ast0.OptIdent(idb))
	  | (_,Ast0.UniqueIdent(idb))
	  | (_,Ast0.MultiIdent(idb)) -> match_ident pattern idb
	  | _ -> return false
	else return false in

  let rec match_expr pattern expr =
    match Ast0.unwrap pattern with
      Ast0.MetaExpr(name,None,pure) ->
	add_pure_binding name pure
	  (function Ast0.MetaExpr(name1,_,true) -> Some name1 | _ -> None)
	  (function expr -> Ast0.ExprTag expr)
	  expr
    | Ast0.MetaExpr(name,Some ts,pure) ->
	let expty = Ast0.get_type expr in
	if List.exists (function t -> Type_cocci.compatible t expty) ts
	then
	  add_pure_binding name pure
	    (function Ast0.MetaExpr(name1,_,true) -> Some name1 | _ -> None)
	    (function expr -> Ast0.ExprTag expr)
	    expr
	else return false
    | Ast0.MetaConst(namea,_,pure) -> failwith "metaconst not supported"
    | Ast0.MetaErr(namea,pure) -> failwith "metaerr not supported"
    | Ast0.MetaExprList(namea,pure) -> failwith "metaexprlist not supported"
    | up ->
	if not(context_required) or is_context expr
	then
	  match (up,Ast0.unwrap expr) with
	    (Ast0.Ident(ida),Ast0.Ident(idb)) ->
	      match_ident ida idb
	  | (Ast0.Constant(consta),Ast0.Constant(constb)) ->
	      return (mcode_equal consta constb)
	  | (Ast0.FunCall(fna,_,argsa,_),Ast0.FunCall(fnb,lp,argsb,rp)) ->
	      conjunct_bindings (match_expr fna fnb)
		(match_dots match_expr is_elist_matcher do_elist_match
		   argsa argsb)
	  | (Ast0.Assignment(lefta,opa,righta),
	     Ast0.Assignment(leftb,opb,rightb)) ->
	       if mcode_equal opa opb
	       then
		 conjunct_bindings (match_expr lefta leftb)
		   (match_expr righta rightb)
	       else return false
	  | (Ast0.CondExpr(exp1a,_,exp2a,_,exp3a),
	     Ast0.CondExpr(exp1b,lp,exp2b,rp,exp3b)) ->
	       conjunct_bindings (match_expr exp1a exp1b)
		 (conjunct_bindings (match_option match_expr exp2a exp2b)
		    (match_expr exp3a exp3b))
	  | (Ast0.Postfix(expa,opa),Ast0.Postfix(expb,opb)) ->
	      if mcode_equal opa opb
	      then match_expr expa expb
	      else return false
	  | (Ast0.Infix(expa,opa),Ast0.Infix(expb,opb)) ->
	      if mcode_equal opa opb
	      then match_expr expa expb
	      else return false
	  | (Ast0.Unary(expa,opa),Ast0.Unary(expb,opb)) ->
	      if mcode_equal opa opb
	      then match_expr expa expb
	      else return false
	  | (Ast0.Binary(lefta,opa,righta),Ast0.Binary(leftb,opb,rightb)) ->
	      if mcode_equal opa opb
	      then
		conjunct_bindings (match_expr lefta leftb)
		  (match_expr righta rightb)
	      else return false
	  | (Ast0.Paren(_,expa,_),Ast0.Paren(lp,expb,rp)) ->
	      match_expr expa expb
	  | (Ast0.ArrayAccess(exp1a,_,exp2a,_),
	     Ast0.ArrayAccess(exp1b,lb,exp2b,rb)) ->
	       conjunct_bindings (match_expr exp1a exp1b)
		 (match_expr exp2a exp2b)
	  | (Ast0.RecordAccess(expa,_,fielda),
	     Ast0.RecordAccess(expb,op,fieldb))
	  | (Ast0.RecordPtAccess(expa,_,fielda),
	     Ast0.RecordPtAccess(expb,op,fieldb)) ->
	       conjunct_bindings (match_ident fielda fieldb)
		 (match_expr expa expb)
	  | (Ast0.Cast(_,tya,_,expa),Ast0.Cast(lp,tyb,rp,expb)) ->
	      conjunct_bindings (match_typeC tya tyb)
		(match_expr expa expb)
	  | (Ast0.SizeOfExpr(_,expa),Ast0.SizeOfExpr(szf,expb)) ->
	      match_expr expa expb
	  | (Ast0.SizeOfType(_,_,tya,_),Ast0.SizeOfType(szf,lp,tyb,rp)) ->
	      match_typeC tya tyb
	  | (Ast0.TypeExp(tya),Ast0.TypeExp(tyb)) ->
	      match_typeC tya tyb
	  | (Ast0.EComma(_),Ast0.EComma(cm)) -> return true
	  | (Ast0.DisjExpr(_,expsa,_,_),_) ->
	      failwith "not allowed in the pattern of an isomorphism"
	  | (Ast0.NestExpr(_,exp_dotsa,_,_),_) ->
	      failwith "not allowed in the pattern of an isomorphism"
	  | (Ast0.Edots(_,None),Ast0.Edots(_,None))
	  | (Ast0.Ecircles(_,None),Ast0.Ecircles(_,None))
	  | (Ast0.Estars(_,None),Ast0.Estars(_,None)) -> return true
	  | (Ast0.Edots(ed,None),Ast0.Edots(_,Some wc))
	  | (Ast0.Ecircles(ed,None),Ast0.Ecircles(_,Some wc))
	  | (Ast0.Estars(ed,None),Ast0.Estars(_,Some wc)) ->
	    (* hope that mcode of edots is unique somehow *)
	      let (edots_whencode_allowed,_,_) = whencode_allowed in
	      if edots_whencode_allowed
	      then add_dot_binding ed (Ast0.ExprTag wc)
	      else
		(Printf.printf "warning: not applying iso because of whencode";
		 return false)
	  | (Ast0.Edots(_,Some _),_) | (Ast0.Ecircles(_,Some _),_)
	  | (Ast0.Estars(_,Some _),_) ->
	      failwith "whencode not allowed in a pattern"
	  | (Ast0.OptExp(expa),Ast0.OptExp(expb))
	  | (Ast0.UniqueExp(expa),Ast0.UniqueExp(expb))
	  | (Ast0.MultiExp(expa),Ast0.MultiExp(expb)) -> match_expr expa expb
	  | (_,Ast0.OptExp(expb))
	  | (_,Ast0.UniqueExp(expb))
	  | (_,Ast0.MultiExp(expb)) -> match_expr pattern expb
	  | _ -> return false
	else return false

  and match_typeC pattern t =
    match Ast0.unwrap pattern with
      Ast0.MetaType(name,pure) ->
	add_pure_binding name pure
	  (function Ast0.MetaType(name1,true) -> Some name1 | _ -> None)
	  (function ty -> Ast0.TypeCTag ty)
	  t
    | up ->
	if not(context_required) or is_context t
	then
	  match (up,Ast0.unwrap t) with
	    (Ast0.ConstVol(cva,tya),Ast0.ConstVol(cvb,tyb)) ->
	      if mcode_equal cva cvb
	      then match_typeC tya tyb
	      else return false
	  | (Ast0.BaseType(tya,signa),Ast0.BaseType(tyb,signb)) ->
	      return (mcode_equal tya tyb &&
		      bool_match_option mcode_equal signa signb)
	  | (Ast0.Pointer(tya,_),Ast0.Pointer(tyb,star)) -> match_typeC tya tyb
	  | (Ast0.Array(tya,_,sizea,_),Ast0.Array(tyb,lb,sizeb,rb)) ->
	      conjunct_bindings (match_typeC tya tyb)
		(match_option match_expr sizea sizeb)
	  | (Ast0.StructUnionName(kinda,namea),
	     Ast0.StructUnionName(kindb,nameb)) ->
	       if mcode_equal kinda kindb
	       then match_ident namea nameb
	       else return false
	  | (Ast0.StructUnionDef(kinda,namea,_,declsa,_),
	     Ast0.StructUnionDef(kindb,nameb,_,declsb,_)) ->
	       if mcode_equal kinda kindb
	       then
		 conjunct_bindings (match_ident namea nameb)
		   (match_list match_decl no_list do_nolist_match
		      declsa declsb)
	       else return false
	  | (Ast0.TypeName(namea),Ast0.TypeName(nameb)) ->
	      return (mcode_equal namea nameb)
	  | (Ast0.OptType(tya),Ast0.OptType(tyb))
	  | (Ast0.UniqueType(tya),Ast0.UniqueType(tyb))
	  | (Ast0.MultiType(tya),Ast0.MultiType(tyb)) -> match_typeC tya tyb
	  | (_,Ast0.OptType(tyb))
	  | (_,Ast0.UniqueType(tyb))
	  | (_,Ast0.MultiType(tyb)) -> match_typeC pattern tyb
	  | _ -> return false
	else return false
	    
  and match_decl pattern d =
    if not(context_required) or is_context d
    then
      match (Ast0.unwrap pattern,Ast0.unwrap d) with
	(Ast0.Init(stga,tya,ida,_,inia,_),Ast0.Init(stgb,tyb,idb,_,inib,_)) ->
	  if bool_match_option mcode_equal stga stgb
	  then
	    conjunct_bindings (match_typeC tya tyb)
	      (conjunct_bindings (match_ident ida idb) (match_init inia inib))
	  else return false
      | (Ast0.UnInit(stga,tya,ida,_),Ast0.UnInit(stgb,tyb,idb,_)) ->
	  if bool_match_option mcode_equal stga stgb
	  then conjunct_bindings (match_typeC tya tyb) (match_ident ida idb)
	  else return false
      | (Ast0.TyDecl(tya,_),Ast0.TyDecl(tyb,_)) -> match_typeC tya tyb
      | (Ast0.DisjDecl(_,declsa,_,_),Ast0.DisjDecl(_,declsb,_,_)) ->
	  failwith "not allowed in the pattern of an isomorphism"
      | (Ast0.OptDecl(decla),Ast0.OptDecl(declb))
      | (Ast0.UniqueDecl(decla),Ast0.UniqueDecl(declb))
      | (Ast0.MultiDecl(decla),Ast0.MultiDecl(declb)) -> match_decl decla declb
      | (_,Ast0.OptDecl(declb))
      | (_,Ast0.UniqueDecl(declb))
      | (_,Ast0.MultiDecl(declb)) -> match_decl pattern declb
      | _ -> return false
    else return false

  and match_init pattern i =
    if not(context_required) or is_context i
    then
      match (Ast0.unwrap pattern,Ast0.unwrap i) with
	(Ast0.InitExpr(expa),Ast0.InitExpr(expb)) ->
	  match_expr expa expb
      | (Ast0.InitList(_,initlista,_),Ast0.InitList(_,initlistb,_)) ->
	  match_dots match_init no_list do_nolist_match initlista initlistb
      | (Ast0.InitGccDotName(_,namea,_,inia),
	 Ast0.InitGccDotName(_,nameb,_,inib)) ->
	   conjunct_bindings (match_ident namea nameb) (match_init inia inib)
      | (Ast0.InitGccName(namea,_,inia),Ast0.InitGccName(nameb,_,inib)) ->
	   conjunct_bindings (match_ident namea nameb) (match_init inia inib)
      | (Ast0.InitGccIndex(_,expa,_,_,inia),
	 Ast0.InitGccIndex(_,expb,_,_,inib)) ->
	   conjunct_bindings (match_expr expa expb) (match_init inia inib)
      | (Ast0.InitGccRange(_,exp1a,_,exp2a,_,_,inia),
	 Ast0.InitGccRange(_,exp1b,_,exp2b,_,_,inib)) ->
	   conjunct_bindings (match_expr exp1a exp1b)
	    (conjunct_bindings (match_expr exp2a exp2b) (match_init inia inib))
      | (Ast0.IComma(_),Ast0.IComma(_)) -> return true
      | (Ast0.Idots(_,None),Ast0.Idots(_,None)) -> return true
      | (Ast0.Idots(id,None),Ast0.Idots(_,Some wc)) ->
	  (* hope that mcode of edots is unique somehow *)
	  let (_,idots_whencode_allowed,_) = whencode_allowed in
	  if idots_whencode_allowed
	  then add_dot_binding id (Ast0.InitTag wc)
	  else
	    (Printf.printf "warning: not applying iso because of whencode";
	     return false)
      | (Ast0.Idots(_,Some _),_) ->
	  failwith "whencode not allowed in a pattern"
      | (Ast0.OptIni(ia),Ast0.OptIni(ib))
      | (Ast0.UniqueIni(ia),Ast0.UniqueIni(ib))
      | (Ast0.MultiIni(ia),Ast0.MultiIni(ib)) -> match_init ia ib
      | (_,Ast0.OptIni(ib))
      | (_,Ast0.UniqueIni(ib))
      | (_,Ast0.MultiIni(ib)) -> match_init pattern ib
      | _ -> return false
    else return false

  and match_param pattern p =
    match Ast0.unwrap pattern with
      Ast0.MetaParam(name,pure) ->
	add_pure_binding name pure
	  (function Ast0.MetaParam(name1,true) -> Some name1 | _ -> None)
	  (function p -> Ast0.ParamTag p)
	  p
    | Ast0.MetaParamList(name,pure) -> failwith "metaparamlist not supported"
    | up ->
	if not(context_required) or is_context p
	then
	  match (up,Ast0.unwrap p) with
	    (Ast0.VoidParam(tya),Ast0.VoidParam(tyb)) -> match_typeC tya tyb
	  | (Ast0.Param(ida,tya),Ast0.Param(idb,tyb)) ->
	      conjunct_bindings (match_typeC tya tyb) (match_ident ida idb)
	  | (Ast0.PComma(_),Ast0.PComma(_))
	  | (Ast0.Pdots(_),Ast0.Pdots(_))
	  | (Ast0.Pcircles(_),Ast0.Pcircles(_)) -> return true
	  | (Ast0.OptParam(parama),Ast0.OptParam(paramb))
	  | (Ast0.UniqueParam(parama),Ast0.UniqueParam(paramb)) ->
	      match_param parama paramb
	  | (_,Ast0.OptParam(paramb))
	  | (_,Ast0.UniqueParam(paramb)) -> match_param pattern paramb
	  | _ -> return false
	else return false
	    
  and match_statement pattern s =
    match Ast0.unwrap pattern with
      Ast0.MetaStmt(name,pure) ->
	add_pure_binding name pure
	  (function Ast0.MetaStmt(name1,true) -> Some name1 | _ -> None)
	  (function ty -> Ast0.StmtTag ty)
	  s
    | Ast0.MetaStmtList(name,pure) -> failwith "metastmtlist not supported"
    | up ->
	if not(context_required) or is_context s
	then
	  match (up,Ast0.unwrap s) with
	    (Ast0.FunDecl(stga,tya,namea,_,paramsa,_,_,bodya,_),
	     Ast0.FunDecl(stgb,tyb,nameb,_,paramsb,_,_,bodyb,_)) ->
	       if bool_match_option mcode_equal stga stgb
	       then
		 conjunct_bindings
		   (match_option match_typeC tya tyb)
		   (conjunct_bindings
		      (match_ident namea nameb)
		      (conjunct_bindings
			 (match_dots
			    match_param is_plist_matcher do_plist_match
			    paramsa paramsb)
			 (match_dots
			    match_statement is_slist_matcher do_slist_match
			    bodya bodyb)))
	       else return false
	  | (Ast0.Decl(decla),Ast0.Decl(declb)) -> match_decl decla declb
	  | (Ast0.Seq(_,bodya,_),Ast0.Seq(_,bodyb,_)) ->
	      match_dots match_statement is_slist_matcher do_slist_match
		bodya bodyb
	  | (Ast0.ExprStatement(expa,_),Ast0.ExprStatement(expb,_)) ->
	      match_expr expa expb
	  | (Ast0.IfThen(_,_,expa,_,branch1a,_),
	     Ast0.IfThen(_,_,expb,_,branch1b,_)) ->
	       conjunct_bindings (match_expr expa expb)
		 (match_statement branch1a branch1b)
	  | (Ast0.IfThenElse(_,_,expa,_,branch1a,_,branch2a,_),
	     Ast0.IfThenElse(_,_,expb,_,branch1b,_,branch2b,_)) ->
	       conjunct_bindings
		 (match_expr expa expb)
		 (conjunct_bindings
		    (match_statement branch1a branch1b)
		    (match_statement branch2a branch2b))
	  | (Ast0.While(_,_,expa,_,bodya,_),Ast0.While(_,_,expb,_,bodyb,_)) ->
	      conjunct_bindings (match_expr expa expb)
		(match_statement bodya bodyb)
	  | (Ast0.Do(_,bodya,_,_,expa,_,_),Ast0.Do(_,bodyb,_,_,expb,_,_)) ->
	      conjunct_bindings (match_statement bodya bodyb)
		(match_expr expa expb)
	  | (Ast0.For(_,_,e1a,_,e2a,_,e3a,_,bodya,_),
	     Ast0.For(_,_,e1b,_,e2b,_,e3b,_,bodyb,_)) ->
	       conjunct_bindings
		 (match_option match_expr e1a e1b)
		 (conjunct_bindings
		    (match_option match_expr e2a e2b)
		    (conjunct_bindings
		       (match_option match_expr e3a e3b)
		       (match_statement bodya bodyb)))
	  | (Ast0.Break(_,_),Ast0.Break(_,_)) -> return true
	  | (Ast0.Continue(_,_),Ast0.Continue(_,_)) -> return true
	  | (Ast0.Return(_,_),Ast0.Return(_,_)) -> return true
	  | (Ast0.ReturnExpr(_,expa,_),Ast0.ReturnExpr(_,expb,_)) ->
	      match_expr expa expb
	  | (Ast0.Disj(_,statement_dots_lista,_,_),_) ->
	      failwith "disj not supported in patterns"
	  | (Ast0.Nest(_,stmt_dotsa,_,_),_) ->
	      failwith "nest not supported in patterns"
	  | (Ast0.Exp(expa),Ast0.Exp(expb)) -> match_expr expa expb
	  | (Ast0.Dots(_,Ast0.NoWhen),Ast0.Dots(_,Ast0.NoWhen))
	  | (Ast0.Circles(_,Ast0.NoWhen),Ast0.Circles(_,Ast0.NoWhen))
	  | (Ast0.Stars(_,Ast0.NoWhen),Ast0.Stars(_,Ast0.NoWhen)) ->
	      return true
	  | (Ast0.Dots(d,Ast0.NoWhen),Ast0.Dots(_,Ast0.WhenNot wc))
	  | (Ast0.Circles(d,Ast0.NoWhen),Ast0.Circles(_,Ast0.WhenNot wc))
	  | (Ast0.Stars(d,Ast0.NoWhen),Ast0.Stars(_,Ast0.WhenNot wc)) ->
	  (* hope that mcode of dots is unique somehow *)
	      let (_,_,dots_whencode_allowed) = whencode_allowed in
	      if dots_whencode_allowed
	      then add_dot_binding d (Ast0.DotsStmtTag wc)
	      else
		(Printf.printf "warning: not applying iso because of whencode";
		 return false)
	  | (Ast0.Dots(d,Ast0.NoWhen),Ast0.Dots(_,Ast0.WhenAlways wc))
	  | (Ast0.Circles(d,Ast0.NoWhen),Ast0.Circles(_,Ast0.WhenAlways wc))
	  | (Ast0.Stars(d,Ast0.NoWhen),Ast0.Stars(_,Ast0.WhenAlways wc)) ->
	  (* hope that mcode of dots is unique somehow *)
	      let (_,_,dots_whencode_allowed) = whencode_allowed in
	      if dots_whencode_allowed
	      then add_dot_binding d (Ast0.StmtTag wc)
	      else
		(Printf.printf "warning: not applying iso because of whencode";
		 return false)
	  | (Ast0.Dots(_,_),_) | (Ast0.Circles(_,_),_) | (Ast0.Stars(_,_),_) ->
	      failwith "whencode not allowed in a pattern"
	  | (Ast0.OptStm(rea),Ast0.OptStm(reb))
	  | (Ast0.UniqueStm(rea),Ast0.UniqueStm(reb))
	  | (Ast0.MultiStm(rea),Ast0.MultiStm(reb)) -> match_statement rea reb
	  | (_,Ast0.OptStm(reb))
	  | (_,Ast0.UniqueStm(reb))
	  | (_,Ast0.MultiStm(reb)) -> match_statement pattern reb
	  |	_ -> return false
	else return false in
(*
  let match_top_level pattern t =
    if not(context_required) or is_context t
    then
      match (Ast0.unwrap pattern,Ast0.unwrap t) with
	(Ast0.DECL(decla),Ast0.DECL(declb)) ->
	  match_decl decla declb
      | (Ast0.INCLUDE(inca,namea),Ast0.INCLUDE(incb,nameb)) ->
	  return (mcode_equal inca incb && mcode_equal namea nameb)
      | (Ast0.FILEINFO(old_filea,new_filea),
	 Ast0.FILEINFO(old_fileb,new_fileb)) ->
	   return (mcode_equal old_filea old_fileb &&
		   mcode_equal new_filea new_fileb)
      | (Ast0.FUNCTION(statementa),Ast0.FUNCTION(statementb)) ->
	  match_statement statementa statementb
      | (Ast0.CODE(stmt_dotsa),Ast0.CODE(stmt_dotsb)) ->
	  match_dots match_statement is_slist_matcher do_slist_match
            stmt_dotsa stmt_dotsb
      | (Ast0.ERRORWORDS(expsa),_) ->
	  failwith "error words not supported in patterns"
      | (Ast0.OTHER(_),_) -> failwith "unexpected code"
      | (_,Ast0.OTHER(_)) -> failwith "unexpected code"
      | _ -> return false
    else return false in
*)
  (match_expr, match_decl, match_statement)

let match_expr context_required whencode_allowed =
  let (fn,_,_) = match_maker context_required whencode_allowed in
  fn

let match_decl context_required whencode_allowed =
  let (_,fn,_) = match_maker context_required whencode_allowed in
  fn

let match_statement context_required whencode_allowed =
  let (_,_,fn) = match_maker context_required whencode_allowed in
  fn

(* --------------------------------------------------------------------- *)
(* make an entire tree MINUS *)

let make_minus =
  let mcode (term,arity,info,mcodekind) =
    (term,arity,info,
     match mcodekind with
       Ast0.CONTEXT(mc) ->
	 (match !mc with
	   (Ast.NOTHING,_,_) -> Ast0.MINUS(ref([],Ast0.default_token_info))
	 | _ -> failwith "make_minus: unexpected befaft")
     | Ast0.MINUS(mc) -> mcodekind (* in the part copied from the src term *)
     | _ -> failwith "make_minus mcode: unexpected mcodekind") in

  let update_mc mcodekind =
    match !mcodekind with
      Ast0.CONTEXT(mc) ->
	(match !mc with
	  (Ast.NOTHING,_,_) ->
	    mcodekind := Ast0.MINUS(ref([],Ast0.default_token_info))
	| _ -> failwith "make_minus: unexpected befaft")
    | Ast0.MINUS(_mc) -> () (* in the part copied from the src term *)
    | _ -> failwith "make_minus donothing: unexpected mcodekind" in

  let donothing r k e =
    let mcodekind = Ast0.get_mcodekind_ref e in
    let e = k e in update_mc mcodekind; e in

  (* special case for whencode, because it isn't processed by contextneg,
     since it doesn't appear in the + code *)
  (* cases for dots and nests *)
  let expression r k e =
    let mcodekind = Ast0.get_mcodekind_ref e in
    match Ast0.unwrap e with
      Ast0.Edots(d,whencode) ->
	(*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind; Ast0.rewrap e (Ast0.Edots(mcode d,whencode))
    | Ast0.Ecircles(d,whencode) ->
	(*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind; Ast0.rewrap e (Ast0.Ecircles(mcode d,whencode))
    | Ast0.Estars(d,whencode) ->
	(*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind; Ast0.rewrap e (Ast0.Estars(mcode d,whencode))
    | Ast0.NestExpr(starter,expr_dots,ender,whencode) ->
	update_mc mcodekind;
	Ast0.rewrap e
	  (Ast0.NestExpr(mcode starter,
			 r.V0.rebuilder_expression_dots expr_dots,
			 mcode ender,whencode))
    | _ -> donothing r k e in

  let statement r k e =
    let mcodekind = Ast0.get_mcodekind_ref e in
    match Ast0.unwrap e with
      Ast0.Dots(d,whencode) ->
	(*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind; Ast0.rewrap e (Ast0.Dots(mcode d,whencode))
    | Ast0.Circles(d,whencode) ->
	update_mc mcodekind; Ast0.rewrap e (Ast0.Circles(mcode d,whencode))
    | Ast0.Stars(d,whencode) ->
	update_mc mcodekind; Ast0.rewrap e (Ast0.Stars(mcode d,whencode))
    | Ast0.Nest(starter,stmt_dots,ender,whencode) ->
	update_mc mcodekind;
	Ast0.rewrap e
	  (Ast0.Nest(mcode starter,r.V0.rebuilder_statement_dots stmt_dots,
		     mcode ender,whencode))
    | _ -> donothing r k e in

  let initialiser r k e =
    let mcodekind = Ast0.get_mcodekind_ref e in
    match Ast0.unwrap e with
      Ast0.Idots(d,whencode) ->
	(*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind; Ast0.rewrap e (Ast0.Idots(mcode d,whencode))
    | _ -> donothing r k e in

  let dots r k e =
    let info = Ast0.get_info e in
    let mcodekind = Ast0.get_mcodekind_ref e in
    match Ast0.unwrap e with
      Ast0.DOTS([]) ->
	(* if context is - this should be - as well.  There are no tokens
	   here though, so the bottom-up minusifier in context_neg leaves it
	   as mixed.  It would be better to fix context_neg, but that would
	   require a special case for each term with a dots subterm. *)
	(match !mcodekind with
	  Ast0.MIXED(mc) ->
	    (match !mc with
	      (Ast.NOTHING,_,_) ->
		mcodekind := Ast0.MINUS(ref([],Ast0.default_token_info));
		e
	    | _ -> failwith "make_minus: unexpected befaft")
	  (* code already processed by an enclosing iso *)
	| Ast0.MINUS(mc) -> e
	| _ ->
	    failwith
	      (Printf.sprintf
		 "%d: make_minus donothingxxx: unexpected mcodekind"
		 info.Ast0.line_start))
    | _ -> donothing r k e in

  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    dots dots dots dots
    donothing expression donothing initialiser donothing donothing
    statement donothing donothing

(* --------------------------------------------------------------------- *)
(* rebuild mcode cells in an instantiated alt *)

(* mcodes will be side effected later with plus code, so we have to copy
them on instantiating an isomorphism.  One could wonder whether it would
be better not to use side-effects, but they are convenient for insert_plus
where is it useful to manipulate a list of the mcodes but side-effect a tree *)
(* hmm... Insert_plus is called before Iso_pattern... *)
let rebuild_mcode start_line =
  let copy_mcodekind = function
      Ast0.CONTEXT(mc) -> Ast0.CONTEXT(ref (!mc))
    | Ast0.MINUS(mc) -> Ast0.MINUS(ref (!mc))
    | Ast0.MIXED(mc) -> Ast0.MIXED(ref (!mc))
    | Ast0.PLUS -> failwith "rebuild_mcode: unexpected plus mcodekind" in

  let mcode (term,arity,info,mcodekind) =
    let info =
      match start_line with
	Some x -> {info with Ast0.line_start = x; Ast0.line_end = x}
      |	None -> info in
    (term,arity,info,copy_mcodekind mcodekind) in

  let copy_one (term,info,index,mcodekind,ty,dots) =
    let info =
      match start_line with
	Some x -> {info with Ast0.line_start = x; Ast0.line_end = x}
      |	None -> info in
    (term,info,ref !index,ref (copy_mcodekind !mcodekind),ty,dots) in

  let donothing r k e = copy_one (k e) in

  (* case for control operators (if, etc) *)
  let statement r k e =
    let s = k e in
    copy_one
      (Ast0.rewrap s
	 (match Ast0.unwrap s with
	   Ast0.IfThen(iff,lp,tst,rp,branch,(info,mc)) ->
	     Ast0.IfThen(iff,lp,tst,rp,branch,(info,copy_mcodekind mc))
	 | Ast0.IfThenElse(iff,lp,tst,rp,branch1,els,branch2,(info,mc)) ->
	     Ast0.IfThenElse(iff,lp,tst,rp,branch1,els,branch2,
	       (info,copy_mcodekind mc))
	 | Ast0.While(whl,lp,exp,rp,body,(info,mc)) ->
	     Ast0.While(whl,lp,exp,rp,body,(info,copy_mcodekind mc))
	 | Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body,(info,mc)) ->
	     Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body,
		      (info,copy_mcodekind mc))
	 | s -> s)) in

  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing
    donothing statement donothing donothing

(* --------------------------------------------------------------------- *)
(* The problem of whencode.  If an isomorphism contains dots in multiple
rules, then the code that is matched cannot contain whencode, because we
won't know which dots it goes with. Should worry about nests, but they
aren't allowed in isomorphisms for the moment. *)

let count_edots =
  let mcode x = 0 in
  let option_default = 0 in
  let bind x y = x + y in
  let donothing r k e = k e in
  let exprfn r k e =
    match Ast0.unwrap e with
      Ast0.Edots(_,_) | Ast0.Ecircles(_,_) | Ast0.Estars(_,_) -> 1
  | _ -> 0 in

  V0.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    donothing exprfn donothing donothing donothing donothing donothing
    donothing donothing

let count_idots =
  let mcode x = 0 in
  let option_default = 0 in
  let bind x y = x + y in
  let donothing r k e = k e in
  let initfn r k e =
    match Ast0.unwrap e with Ast0.Idots(_,_) -> 1 | _ -> 0 in

  V0.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    donothing donothing donothing initfn donothing donothing donothing
    donothing donothing

let count_dots =
  let mcode x = 0 in
  let option_default = 0 in
  let bind x y = x + y in
  let donothing r k e = k e in
  let stmtfn r k e =
    match Ast0.unwrap e with
      Ast0.Dots(_,_) | Ast0.Circles(_,_) | Ast0.Stars(_,_) -> 1
  | _ -> 0 in

  V0.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing stmtfn
    donothing donothing

(* --------------------------------------------------------------------- *)

let lookup name bindings mv_bindings =
  try Common.Left (List.assoc (term name) bindings)
  with
    Not_found ->
      (* failure is not possible anymore *)
      Common.Right (List.assoc (term name) mv_bindings)

let instantiate bindings mv_bindings =
  let mcode x = x in
  let donothing r k e = k e in

  (* cases where metavariables can occur *)
  let identfn r k e =
    match Ast0.unwrap e with
      Ast0.MetaId(name,pure) ->
	(rebuild_mcode None).V0.rebuilder_ident
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.IdentTag(id)) -> id
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaId(Ast0.set_mcode_data new_mv name,pure)))
    | Ast0.MetaFunc(name,pure) -> failwith "metafunc not supported"
    | Ast0.MetaLocalFunc(name,pure) -> failwith "metalocalfunc not supported"
    | _ -> k e in

  let rec elist r same_dots = function
      [] -> []
    | [x] ->
	(match Ast0.unwrap x with
	  Ast0.MetaExprList(name,pure) ->
	    (match lookup name bindings mv_bindings with
	      Common.Left(Ast0.DotsExprTag(exp)) ->
		(match same_dots exp with
		  Some l -> l
		| None -> failwith "dots put in incompatible context")
	    | Common.Left(Ast0.ExprTag(exp)) -> [exp]
	    | Common.Left(_) -> failwith "not possible 1"
	    | Common.Right(new_mv) ->
		failwith "MetaExprList in SP not supported")
	| _ -> [r.V0.rebuilder_expression x])
    | x::xs -> (r.V0.rebuilder_expression x)::(elist r same_dots xs) in

  let rec plist r same_dots = function
      [] -> []
    | [x] ->
	(match Ast0.unwrap x with
	  Ast0.MetaParamList(name,pure) ->
	    (match lookup name bindings mv_bindings with
	      Common.Left(Ast0.DotsParamTag(param)) ->
		(match same_dots param with
		  Some l -> l
		| None -> failwith "dots put in incompatible context")
	    | Common.Left(Ast0.ParamTag(param)) -> [param]
	    | Common.Left(_) -> failwith "not possible 1"
	    | Common.Right(new_mv) ->
		failwith "MetaExprList in SP not supported")
	| _ -> [r.V0.rebuilder_parameter x])
    | x::xs -> (r.V0.rebuilder_parameter x)::(plist r same_dots xs) in

  let rec slist r same_dots = function
      [] -> []
    | [x] ->
	(match Ast0.unwrap x with
	  Ast0.MetaStmtList(name,pure) ->
	    (match lookup name bindings mv_bindings with
	      Common.Left(Ast0.DotsStmtTag(stm)) ->
		(match same_dots stm with
		  Some l -> l
		| None -> failwith "dots put in incompatible context")
	    | Common.Left(Ast0.StmtTag(stm)) -> [stm]
	    | Common.Left(_) -> failwith "not possible 1"
	    | Common.Right(new_mv) ->
		failwith "MetaExprList in SP not supported")
	| _ -> [r.V0.rebuilder_statement x])
    | x::xs -> (r.V0.rebuilder_statement x)::(slist r same_dots xs) in

  let same_dots d =
    match Ast0.unwrap d with Ast0.DOTS(l) -> Some l |_ -> None in
  let same_circles d =
    match Ast0.unwrap d with Ast0.CIRCLES(l) -> Some l |_ -> None in
  let same_stars d =
    match Ast0.unwrap d with Ast0.STARS(l) -> Some l |_ -> None in

  let dots list_fn r k d =
    Ast0.rewrap d
      (match Ast0.unwrap d with
	Ast0.DOTS(l) -> Ast0.DOTS(list_fn r same_dots l)
      | Ast0.CIRCLES(l) -> Ast0.CIRCLES(list_fn r same_circles l)
      | Ast0.STARS(l) -> Ast0.STARS(list_fn r same_stars l)) in

  let exprfn r k e =
    match Ast0.unwrap e with
      Ast0.MetaExpr(name,x,pure) ->
	(rebuild_mcode None).V0.rebuilder_expression
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.ExprTag(exp)) -> exp
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaExpr(Ast0.set_mcode_data new_mv name,x,pure)))
    | Ast0.MetaConst(namea,_,pure) -> failwith "metaconst not supported"
    | Ast0.MetaErr(namea,pure) -> failwith "metaerr not supported"
    | Ast0.MetaExprList(namea,pure) -> failwith "metaexprlist not supported"
    | Ast0.Edots(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.ExprTag(exp) -> Ast0.rewrap e (Ast0.Edots(d,Some exp))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | Ast0.Ecircles(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.ExprTag(exp) -> Ast0.rewrap e (Ast0.Ecircles(d,Some exp))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | Ast0.Estars(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.ExprTag(exp) -> Ast0.rewrap e (Ast0.Estars(d,Some exp))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | _ -> k e in

  let tyfn r k e =
    match Ast0.unwrap e with
      Ast0.MetaType(name,pure) ->
	(rebuild_mcode None).V0.rebuilder_typeC
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.TypeCTag(ty)) -> ty
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaType(Ast0.set_mcode_data new_mv name,pure)))
    | _ -> k e in

  let paramfn r k e =
    match Ast0.unwrap e with
      Ast0.MetaParam(name,pure) ->
	(rebuild_mcode None).V0.rebuilder_parameter
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.ParamTag(param)) -> param
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaParam(Ast0.set_mcode_data new_mv name,pure)))
    | Ast0.MetaParamList(name,pure) -> failwith "metaparamlist not supported"
    | _ -> k e in

  let stmtfn r k e =
    match Ast0.unwrap e with
    Ast0.MetaStmt(name,pure) ->
	(rebuild_mcode None).V0.rebuilder_statement
	  (match lookup name bindings mv_bindings with
	    Common.Left(Ast0.StmtTag(stm)) -> stm
	  | Common.Left(_) -> failwith "not possible 1"
	  | Common.Right(new_mv) ->
	      Ast0.rewrap e
		(Ast0.MetaStmt(Ast0.set_mcode_data new_mv name,pure)))
    | Ast0.MetaStmtList(name,pure) -> failwith "metastmtlist not supported"
    | Ast0.Dots(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.DotsStmtTag(stms) ->
	      Ast0.rewrap e (Ast0.Dots(d,Ast0.WhenNot stms))
	  | Ast0.StmtTag(stm) ->
	      Ast0.rewrap e (Ast0.Dots(d,Ast0.WhenAlways stm))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | Ast0.Circles(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.DotsStmtTag(stms) ->
	      Ast0.rewrap e (Ast0.Circles(d,Ast0.WhenNot stms))
	  | Ast0.StmtTag(stm) ->
	      Ast0.rewrap e (Ast0.Circles(d,Ast0.WhenAlways stm))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | Ast0.Stars(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.DotsStmtTag(stms) ->
	      Ast0.rewrap e (Ast0.Stars(d,Ast0.WhenNot stms))
	  | Ast0.StmtTag(stm) ->
	      Ast0.rewrap e (Ast0.Stars(d,Ast0.WhenAlways stm))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | _ -> k e in

  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    (dots elist) donothing (dots plist) (dots slist)
    identfn exprfn tyfn donothing paramfn donothing stmtfn donothing
    donothing

(* --------------------------------------------------------------------- *)

let is_minus e =
  match Ast0.get_mcodekind e with Ast0.MINUS(cell) -> true | _ -> false

let context_required e = not(is_minus e)

let disj_fail bindings e =
  match bindings with
    Some x -> Printf.fprintf stderr "no disj available at this type"; e
  | None -> e

(* isomorphism code is by default CONTEXT *)
let copy_plus printer minusify model e =
  match Ast0.get_mcodekind model with
    Ast0.MINUS(mc) ->
      (* minusify e *)
      let e = minusify e in
      (* add the replacement information at the root *)
      (match Ast0.get_mcodekind e with
	Ast0.MINUS(emc) ->
	  emc :=
	    (match (!mc,!emc) with
	      (([],_),(x,t)) | ((x,_),([],t)) -> (x,t)
	    | _ -> failwith "how can we combine minuses?")
      |	_ -> failwith "not possible 6");
      e
  | Ast0.CONTEXT(mc) ->
      (match Ast0.get_mcodekind e with
	Ast0.CONTEXT(emc) ->
	  let (mba,_,_) = !mc in
	  let (eba,tb,ta) = !emc in
	  (* merging may be required when a term is replaced by a subterm *)
	  let merged =
	    match (mba,eba) with
	      (x,Ast.NOTHING) | (Ast.NOTHING,x) -> x
	    | (Ast.BEFORE(b1),Ast.BEFORE(b2)) -> Ast.BEFORE(b1@b2)
	    | (Ast.BEFORE(b),Ast.AFTER(a)) -> Ast.BEFOREAFTER(b,a)
	    | (Ast.BEFORE(b1),Ast.BEFOREAFTER(b2,a)) ->
		Ast.BEFOREAFTER(b1@b2,a)
	    | (Ast.AFTER(a),Ast.BEFORE(b)) -> Ast.BEFOREAFTER(b,a)
	    | (Ast.AFTER(a1),Ast.AFTER(a2)) ->Ast.AFTER(a2@a1)
	    | (Ast.AFTER(a1),Ast.BEFOREAFTER(b,a2)) -> Ast.BEFOREAFTER(b,a2@a1)
	    | (Ast.BEFOREAFTER(b1,a),Ast.BEFORE(b2)) ->
		Ast.BEFOREAFTER(b1@b2,a)
	    | (Ast.BEFOREAFTER(b,a1),Ast.AFTER(a2)) ->
		Ast.BEFOREAFTER(b,a2@a1)
	    | (Ast.BEFOREAFTER(b1,a1),Ast.BEFOREAFTER(b2,a2)) ->
		 Ast.BEFOREAFTER(b1@b2,a2@a1) in
	  emc := (merged,tb,ta)
      |	Ast0.MINUS(emc) ->
	  let (anything_bef_aft,_,_) = !mc in
	  let (anythings,t) = !emc in
	  emc :=
	    (match anything_bef_aft with
	      Ast.BEFORE(b) -> (b@anythings,t)
	    | Ast.AFTER(a) -> (anythings@a,t)
	    | Ast.BEFOREAFTER(b,a) -> (b@anythings@a,t)
	    | Ast.NOTHING -> (anythings,t))
      |	_ -> failwith "not possible 7");
      e
  | Ast0.MIXED(_) -> failwith "not possible 8"
  | Ast0.PLUS -> failwith "not possible 9"

let whencode_allowed prev_ecount prev_icount prev_dcount
    ecount icount dcount rest =
  (* actually, if ecount or dcount is 0, the flag doesn't matter, because it
     won't be tested *)
  let other_ecount = (* number of edots *)
    List.fold_left (function rest -> function (_,ec,ic,dc) -> ec + rest)
      prev_ecount rest in
  let other_icount = (* number of dots *)
    List.fold_left (function rest -> function (_,ec,ic,dc) -> ic + rest)
      prev_icount rest in
  let other_dcount = (* number of dots *)
    List.fold_left (function rest -> function (_,ec,ic,dc) -> dc + rest)
      prev_dcount rest in
  (ecount = 0 or other_ecount = 0, icount = 0 or other_icount = 0,
   dcount = 0 or other_dcount = 0)

(* --------------------------------------------------------------------- *)

let mv_count = ref 0
let new_mv s =
  let ct = !mv_count in
  mv_count := !mv_count + 1;
  "_"^s^"_"^(string_of_int ct)

let get_name = function
    Ast.MetaIdDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaIdDecl(ar,nm))
  | Ast.MetaFreshIdDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaFreshIdDecl(ar,nm))
  | Ast.MetaTypeDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaTypeDecl(ar,nm))
  | Ast.MetaParamDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaParamDecl(ar,nm))
  | Ast.MetaParamListDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaParamListDecl(ar,nm))
  | Ast.MetaConstDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaConstDecl(ar,nm))
  | Ast.MetaErrDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaErrDecl(ar,nm))
  | Ast.MetaExpDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaExpDecl(ar,nm))
  | Ast.MetaExpListDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaExpListDecl(ar,nm))
  | Ast.MetaStmDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaStmDecl(ar,nm))
  | Ast.MetaStmListDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaStmListDecl(ar,nm))
  | Ast.MetaFuncDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaFuncDecl(ar,nm))
  | Ast.MetaLocalFuncDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaLocalFuncDecl(ar,nm))
  | Ast.MetaTextDecl(ar,nm) ->
      (nm,function nm -> Ast.MetaTextDecl(ar,nm))

let make_new_metavars metavars bindings =
  let new_metavars =
    List.filter
      (function mv ->
	let (s,_) = get_name mv in
	try let _ = List.assoc s bindings in false with Not_found -> true)
      metavars in
  List.split
    (List.map
       (function mv ->
	 let (s,rebuild) = get_name mv in
	 let new_s = new_mv s in
	 (rebuild new_s, (s,new_s)))
       new_metavars)

(* --------------------------------------------------------------------- *)

let mkdisj matcher metavars alts instantiater e disj_maker minusify
    rebuild_mcodes printer =
  let call_instantiate bindings mv_bindings alts =
    List.map
      (function (a,_,_,_) ->
	copy_plus printer minusify e
	  (instantiater bindings mv_bindings (rebuild_mcodes a)))
      alts in
  let rec inner_loop all_alts prev_ecount prev_icount prev_dcount = function
      [] -> Common.Left (prev_ecount, prev_icount, prev_dcount)
    | ((pattern,ecount,icount,dcount)::rest) ->
	let wc =
	  whencode_allowed prev_ecount prev_icount prev_dcount
	    ecount dcount icount rest in
	(match matcher (context_required e) wc pattern e init_env with
	  None ->
	    inner_loop all_alts (prev_ecount + ecount) (prev_icount + icount)
	      (prev_dcount + dcount) rest
	| Some bindings ->
	    (match List.concat all_alts with
	      [x] -> Common.Left (prev_ecount, prev_icount, prev_dcount)
	    | all_alts ->
		let (new_metavars,mv_bindings) =
		  make_new_metavars metavars bindings in
		Common.Right
		  (new_metavars,
		   call_instantiate bindings mv_bindings all_alts))) in
  let rec outer_loop prev_ecount prev_icount prev_dcount = function
      [] -> ([],e) (* nothing matched *)
    | (alts::rest) as all_alts ->
	match inner_loop all_alts prev_ecount prev_icount prev_dcount alts with
	  Common.Left(prev_ecount, prev_icount, prev_dcount) ->
	    outer_loop prev_ecount prev_icount prev_dcount rest
	| Common.Right (new_metavars,res) -> (new_metavars,disj_maker res) in
  outer_loop 0 0 0 alts

(* no one should ever look at the information stored in these mcodes *)
let disj_starter =
  ("(",Ast0.NONE,Ast0.default_info(),Ast0.context_befaft())

let disj_ender =
  ("(",Ast0.NONE,Ast0.default_info(),Ast0.context_befaft())

let disj_mid _ =
  ("|",Ast0.NONE,Ast0.default_info(),Ast0.context_befaft())

let make_disj_expr el =
  let mids =
    match el with
      [] -> failwith "bad disjunction"
    | x::xs -> List.map disj_mid xs in
  Ast0.context_wrap (Ast0.DisjExpr(disj_starter,el,mids,disj_ender))
let make_disj_decl dl =
  let mids =
    match dl with
      [] -> failwith "bad disjunction"
    | x::xs -> List.map disj_mid xs in
  Ast0.context_wrap (Ast0.DisjDecl(disj_starter,dl,mids,disj_ender))
let make_disj_stmt sl =
  let dotify x = Ast0.context_wrap (Ast0.DOTS[x]) in
  let mids =
    match sl with
      [] -> failwith "bad disjunction"
    | x::xs -> List.map disj_mid xs in
  Ast0.context_wrap
    (Ast0.Disj(disj_starter,List.map dotify sl,mids,disj_ender))

let transform_expr (metavars,alts) e =
  match alts with
    (Ast0.ExprTag(_)::_)::_ ->
      (* start line is given to any leaves in the iso code *)
      let start_line = Some ((Ast0.get_info e).Ast0.line_start) in
      let alts =
	List.map
	  (List.map
	     (function
		 Ast0.ExprTag(p) ->
		   (p,count_edots.V0.combiner_expression p,
		    count_idots.V0.combiner_expression p,
		    count_dots.V0.combiner_expression p)
	       | _ -> failwith "invalid alt"))
	  alts in
      mkdisj match_expr metavars alts
	(function b -> function mv_b ->
	  (instantiate b mv_b).V0.rebuilder_expression) e
	make_disj_expr make_minus.V0.rebuilder_expression
	(rebuild_mcode start_line).V0.rebuilder_expression
	Unparse_ast0.expression
  | _ -> ([],e)

let transform_decl (metavars,alts) e =
  match alts with
    (Ast0.DeclTag(_)::_)::_ ->
      (* start line is given to any leaves in the iso code *)
      let start_line = Some (Ast0.get_info e).Ast0.line_start in
      let alts =
	List.map
	  (List.map
	     (function
		 Ast0.DeclTag(p) ->
		   (p,count_edots.V0.combiner_declaration p,
		    count_idots.V0.combiner_declaration p,
		    count_dots.V0.combiner_declaration p)
	       | _ -> failwith "invalid alt"))
	  alts in
      mkdisj match_decl metavars alts
	(function b -> function mv_b ->
	  (instantiate b mv_b).V0.rebuilder_declaration) e
	make_disj_decl make_minus.V0.rebuilder_declaration
	(rebuild_mcode start_line).V0.rebuilder_declaration
	Unparse_ast0.declaration
  | _ -> ([],e)

let transform_stmt (metavars,alts) e =
  match alts with
    (Ast0.StmtTag(_)::_)::_ ->
      (* start line is given to any leaves in the iso code *)
      let start_line = Some (Ast0.get_info e).Ast0.line_start in
      let alts =
	List.map
	  (List.map
	     (function
		 Ast0.StmtTag(p) ->
		   (p,count_edots.V0.combiner_statement p,
		    count_idots.V0.combiner_statement p,
		    count_dots.V0.combiner_statement p)
	       | _ -> failwith "invalid alt"))
	  alts in
      mkdisj match_statement metavars alts
	(function b -> function mv_b ->
	  (instantiate b mv_b).V0.rebuilder_statement) e
	make_disj_stmt make_minus.V0.rebuilder_statement
	(rebuild_mcode start_line).V0.rebuilder_statement
	(Unparse_ast0.statement "")
  | _ -> ([],e)

(* --------------------------------------------------------------------- *)

let transform (alts : isomorphism) t =
  (* the following ugliness is because rebuilder only returns a new term *)
  let extra_meta_decls = ref ([] : Ast_cocci.metavar list) in
  let mcode x = x in
  let donothing r k e = k e in
  let exprfn r k e =
    let (extra_meta,exp) = transform_expr alts (k e) in
    extra_meta_decls := extra_meta @ !extra_meta_decls;
    exp in

  let declfn r k e =
    let (extra_meta,dec) = transform_decl alts (k e) in
    extra_meta_decls := extra_meta @ !extra_meta_decls;
    dec in

  let stmtfn r k e =
    let (extra_meta,stm) = transform_stmt alts (k e) in
    extra_meta_decls := extra_meta @ !extra_meta_decls;
    stm in
  
  let res =
    V0.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing
      donothing exprfn donothing donothing donothing declfn stmtfn
      donothing donothing in
  let res = res.V0.rebuilder_top_level t in
  (!extra_meta_decls,res)

(* --------------------------------------------------------------------- *)

(* should be done by functorizing the parser to use wrap or context_wrap *)
let rewrap =
  let mcode (x,a,i,mc) = (x,a,i,Ast0.context_befaft()) in
  let donothing r k e = Ast0.context_wrap(Ast0.unwrap(k e)) in
  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing

let rewrap_anything = function
    Ast0.DotsExprTag(d) ->
      Ast0.DotsExprTag(rewrap.V0.rebuilder_expression_dots d)
  | Ast0.DotsInitTag(d) ->
      Ast0.DotsInitTag(rewrap.V0.rebuilder_initialiser_list d)
  | Ast0.DotsParamTag(d) ->
      Ast0.DotsParamTag(rewrap.V0.rebuilder_parameter_list d)
  | Ast0.DotsStmtTag(d) ->
      Ast0.DotsStmtTag(rewrap.V0.rebuilder_statement_dots d)
  | Ast0.IdentTag(d) -> Ast0.IdentTag(rewrap.V0.rebuilder_ident d)
  | Ast0.ExprTag(d) -> Ast0.ExprTag(rewrap.V0.rebuilder_expression d)
  | Ast0.TypeCTag(d) -> Ast0.TypeCTag(rewrap.V0.rebuilder_typeC d)
  | Ast0.InitTag(d) -> Ast0.InitTag(rewrap.V0.rebuilder_initialiser d)
  | Ast0.ParamTag(d) -> Ast0.ParamTag(rewrap.V0.rebuilder_parameter d)
  | Ast0.DeclTag(d) -> Ast0.DeclTag(rewrap.V0.rebuilder_declaration d)
  | Ast0.StmtTag(d) -> Ast0.StmtTag(rewrap.V0.rebuilder_statement d)
  | Ast0.MetaTag(d) -> Ast0.MetaTag(rewrap.V0.rebuilder_meta d)
  | Ast0.TopTag(d) -> Ast0.TopTag(rewrap.V0.rebuilder_top_level d)

(* --------------------------------------------------------------------- *)

let apply_isos isos rule =
  let isos =
    List.map
      (function (metavars,iso) ->
	(metavars,List.map (List.map rewrap_anything) iso))
      isos in
  let (extra_meta,rule) =
    List.split
      (List.map
	 (function t ->
	   List.fold_left
	     (function (extra_meta,t) -> function iso ->
	       let (new_extra_meta,t) = transform iso t in
	       (new_extra_meta@extra_meta,t))
	     ([],t) isos)
       rule) in
  (List.concat extra_meta, Compute_lines.compute_lines rule)
