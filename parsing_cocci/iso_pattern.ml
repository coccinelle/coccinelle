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

type isomorphism = Ast0_cocci.anything list list

let strip_info =
  let mcode (term,_,_,_) = (term,Ast0.NONE,Ast0.default_info(),Ast0.PLUS) in
  let donothing r k e =
    let (term,info,index,mc,ty) = k e in
    (term,Ast0.default_info(),ref 0,ref Ast0.PLUS,ref None) in
  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing

let anything_equal = function
    (Ast0.DotsExprTag(d1),Ast0.DotsExprTag(d2)) ->
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
  | (Ast0.ParamTag(d1),Ast0.ParamTag(d2)) ->
      (strip_info.V0.rebuilder_parameter d1) =
      (strip_info.V0.rebuilder_parameter d2)
  | (Ast0.DeclTag(d1),Ast0.DeclTag(d2)) ->
      (strip_info.V0.rebuilder_declaration d1) =
      (strip_info.V0.rebuilder_declaration d2)
  | (Ast0.StmtTag(d1),Ast0.StmtTag(d2)) ->
      (strip_info.V0.rebuilder_statement d1) =
      (strip_info.V0.rebuilder_statement d2)
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
  Printf.printf "calling dot_binding for %s\n" var;
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

let is_context (_,_,_,mc,_) =
  match !mc with Ast0.CONTEXT(cell) -> true | _ -> false

let match_list fn la lb =
  if List.length la = List.length lb
  then
    List.fold_left2
      (function rest -> function cura -> function curb ->
	conjunct_bindings (fn cura curb) rest)
      (return true) la lb
  else return false

let match_maker context_required whencode_allowed =

  let match_dots fn d1 d2 =
    if not(context_required) or is_context d2
    then
      match (Ast0.unwrap d1, Ast0.unwrap d2) with
	(Ast0.DOTS(la),Ast0.DOTS(lb))
      | (Ast0.CIRCLES(la),Ast0.CIRCLES(lb))
      | (Ast0.STARS(la),Ast0.STARS(lb)) -> match_list fn la lb
      | _ -> return false
    else return false in

  let rec match_ident pattern id =
    match Ast0.unwrap pattern with
      Ast0.MetaId(name,_) -> add_binding name (Ast0.IdentTag id)
    | Ast0.MetaFunc(name,_) -> failwith "metafunc not supported"
    | Ast0.MetaLocalFunc(name,_) -> failwith "metalocalfunc not supported"
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
      Ast0.MetaExpr(name,None,_) -> add_binding name (Ast0.ExprTag expr)
    | Ast0.MetaExpr(name,Some ts,_) ->
	let expty = Ast0.get_type expr in
	if List.exists (function t -> Type_cocci.compatible t expty) ts
	then add_binding name (Ast0.ExprTag expr)
	else return false
    | Ast0.MetaConst(namea,_,_) -> failwith "metaconst not supported"
    | Ast0.MetaErr(namea,_) -> failwith "metaerr not supported"
    | Ast0.MetaExprList(namea,_) -> failwith "metaexprlist not supported"
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
		(match_dots match_expr argsa argsb)
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
	  | (Ast0.EComma(_),Ast0.EComma(cm)) -> return true
	  | (Ast0.DisjExpr(_,expsa,_),_) ->
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
	      let (edots_whencode_allowed,_) = whencode_allowed in
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
      Ast0.MetaType(name,_) -> add_binding name (Ast0.TypeCTag t)
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
	  | (Ast0.StructUnionName(namea,kinda),
	     Ast0.StructUnionName(nameb,kindb)) ->
	       return (mcode_equal namea nameb && mcode_equal kinda kindb)
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
	(Ast0.Init(tya,ida,_,expa,_),Ast0.Init(tyb,idb,_,expb,_)) ->
	  conjunct_bindings (match_typeC tya tyb)
	    (conjunct_bindings (match_ident ida idb) (match_expr expa expb))
      | (Ast0.UnInit(tya,ida,_),Ast0.UnInit(tyb,idb,_)) ->
	  conjunct_bindings (match_typeC tya tyb) (match_ident ida idb)
      | (Ast0.DisjDecl(_,declsa,_),Ast0.DisjDecl(_,declsb,_)) ->
	  failwith "not allowed in the pattern of an isomorphism"
      | (Ast0.OptDecl(decla),Ast0.OptDecl(declb))
      | (Ast0.UniqueDecl(decla),Ast0.UniqueDecl(declb))
      | (Ast0.MultiDecl(decla),Ast0.MultiDecl(declb)) -> match_decl decla declb
      | (_,Ast0.OptDecl(declb))
      | (_,Ast0.UniqueDecl(declb))
      | (_,Ast0.MultiDecl(declb)) -> match_decl pattern declb
      | _ -> return false
    else return false
	
  and match_param pattern p =
    match Ast0.unwrap pattern with
      Ast0.MetaParam(name,_) -> add_binding name (Ast0.ParamTag p)
    | Ast0.MetaParamList(name,_) -> failwith "metaparamlist not supported"
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
      Ast0.MetaStmt(name,_) -> add_binding name (Ast0.StmtTag s)
    | Ast0.MetaStmtList(name,_) -> failwith "metastmtlist not supported"
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
			 (match_dots match_param paramsa paramsb)
			 (match_dots match_statement bodya bodyb)))
	       else return false
	  | (Ast0.Decl(decla),Ast0.Decl(declb)) -> match_decl decla declb
	  | (Ast0.Seq(_,bodya,_),Ast0.Seq(_,bodyb,_)) ->
	      match_dots match_statement bodya bodyb
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
	  | (Ast0.Disj(_,statement_dots_lista,_),_) ->
	      failwith "disj not supported in patterns"
	  | (Ast0.Nest(_,stmt_dotsa,_,_),_) ->
	      failwith "nest not supported in patterns"
	  | (Ast0.Exp(expa),Ast0.Exp(expb)) -> match_expr expa expb
	  | (Ast0.Dots(_,None),Ast0.Dots(_,None))
	  | (Ast0.Circles(_,None),Ast0.Circles(_,None))
	  | (Ast0.Stars(_,None),Ast0.Stars(_,None)) -> return true
	  | (Ast0.Dots(d,None),Ast0.Dots(_,Some wc))
	  | (Ast0.Circles(d,None),Ast0.Circles(_,Some wc))
	  | (Ast0.Stars(d,None),Ast0.Stars(_,Some wc)) ->
	  (* hope that mcode of dots is unique somehow *)
	      let (_,dots_whencode_allowed) = whencode_allowed in
	      if dots_whencode_allowed
	      then add_dot_binding d (Ast0.DotsStmtTag wc)
	      else
		(Printf.printf "warning: not applying iso because of whencode";
		 return false)
	  | (Ast0.Dots(_,Some _),_) | (Ast0.Circles(_,Some _),_)
	  | (Ast0.Stars(_,Some _),_) ->
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
	  match_dots match_statement stmt_dotsa stmt_dotsb
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

  let donothing r k ((term,info,index,mcodekind,ty) as e) =
    let e = k e in update_mc mcodekind; e in

  (* special case for whencode, because it isn't processed by contextneg,
     since it doesn't appear in the + code *)
  let expression r k ((term,info,index,mcodekind,ty) as e) =
    match term with
      Ast0.Edots(dots,whencode) ->
	(*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind; Ast0.rewrap e (Ast0.Edots(dots,whencode))
    | Ast0.Ecircles(dots,whencode) ->
	(*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind; Ast0.rewrap e (Ast0.Ecircles(dots,whencode))
    | Ast0.Estars(dots,whencode) ->
	(*don't recurse because whencode hasn't been processed by context_neg*)
	update_mc mcodekind; Ast0.rewrap e (Ast0.Estars(dots,whencode))
    | Ast0.NestExpr(starter,expr_dots,ender,whencode) ->
	update_mc mcodekind;
	Ast0.rewrap e
	  (Ast0.NestExpr(mcode starter,
			 r.V0.rebuilder_expression_dots expr_dots,
			 mcode ender,whencode))
    | _ -> donothing r k e in

  let statement r k ((term,info,index,mcodekind,ty) as e) =
    match term with
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

  let dots r k ((term,info,index,mcodekind,ty) as e) =
    match term with
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
    dots dots dots
    donothing expression donothing donothing donothing
    statement donothing

(* --------------------------------------------------------------------- *)
(* rebuild mcode cells in an instantiated alt *)

(* mcodes will be side effected later with plus code, so we have to copy
then on instantiating an isomorphism.  One could wonder whether it would
be better not to use side-effects, but they are convenient for insert_plus
where is it useful to manipulate a list of the mcodes but side-effect a tree *)
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

  let donothing r k e =
    let (term,info,index,mcodekind,ty) = k e in
    let info =
      match start_line with
	Some x -> {info with Ast0.line_start = x; Ast0.line_end = x}
      |	None -> info in
    (term,info,ref !index,ref (copy_mcodekind !mcodekind),ty) in

  let statement r k e =
    let s = k e in
    Ast0.rewrap s
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
      | s -> s) in

  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing
    donothing donothing donothing donothing donothing statement donothing

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
    donothing donothing donothing
    donothing exprfn donothing donothing donothing donothing donothing

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
    donothing donothing donothing
    donothing donothing donothing donothing donothing stmtfn donothing

(* --------------------------------------------------------------------- *)

let instantiate bindings =
  let mcode x = x in
  let donothing r k e = k e in

  let line e = (Ast0.get_info e).Ast0.line_start in

  (* cases where metavariables can occur *)
  let identfn r k e =
    match Ast0.unwrap e with
      Ast0.MetaId(name,_) ->
	(try
	  (match List.assoc (term name) bindings with
	    Ast0.IdentTag(id) -> (rebuild_mcode None).V0.rebuilder_ident id
	  | _ -> failwith "not possible 1")
	with
	  Not_found ->
	    failwith (Printf.sprintf "bad variable on line %d\n" (line e)))
    | Ast0.MetaFunc(name,_) -> failwith "metafunc not supported"
    | Ast0.MetaLocalFunc(name,_) -> failwith "metalocalfunc not supported"
    | _ -> k e in

  let exprfn r k e =
    match Ast0.unwrap e with
      Ast0.MetaExpr(name,_,_) ->
	(try
	  (match List.assoc (term name) bindings with
	    Ast0.ExprTag(exp) ->
	      (rebuild_mcode None).V0.rebuilder_expression exp
	  | _ -> failwith "not possible 2")
	with
	  Not_found ->
	    failwith (Printf.sprintf "bad variable on line %d\n" (line e)))
    | Ast0.MetaConst(namea,_,_) -> failwith "metaconst not supported"
    | Ast0.MetaErr(namea,_) -> failwith "metaerr not supported"
    | Ast0.MetaExprList(namea,_) -> failwith "metaexprlist not supported"
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
      Ast0.MetaType(name,_) ->
	(try
	  (match List.assoc (term name) bindings with
	    Ast0.TypeCTag(ty) -> (rebuild_mcode None).V0.rebuilder_typeC ty
	  | _ -> failwith "not possible 3")
	with
	  Not_found ->
	    failwith (Printf.sprintf "bad variable on line %d\n" (line e)))
    | _ -> k e in

  let paramfn r k e =
    match Ast0.unwrap e with
      Ast0.MetaParam(name,_) ->
	(try
	  (match List.assoc (term name) bindings with
	    Ast0.ParamTag(param) ->
	      (rebuild_mcode None).V0.rebuilder_parameter param
	  | _ -> failwith "not possible 4")
	with
	  Not_found ->
	    failwith (Printf.sprintf "bad variable on line %d\n" (line e)))
    | Ast0.MetaParamList(name,_) -> failwith "metaparamlist not supported"
    | _ -> k e in

  let stmtfn r k e =
    match Ast0.unwrap e with
    Ast0.MetaStmt(name,_) ->
	(try
	  (match List.assoc (term name) bindings with
	    Ast0.StmtTag(stmt) ->
	      (rebuild_mcode None).V0.rebuilder_statement stmt
	  | _ -> failwith "not possible 5")
	with
	  Not_found ->
	    failwith (Printf.sprintf "bad variable on line %d\n" (line e)))
    | Ast0.MetaStmtList(name,_) -> failwith "metastmtlist not supported"
    | Ast0.Dots(d,_) ->
	Printf.printf "trying to find whencode for %s\n"
	  (dot_term d);
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.DotsStmtTag(stms) -> Ast0.rewrap e (Ast0.Dots(d,Some stms))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | Ast0.Circles(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.DotsStmtTag(stms) -> Ast0.rewrap e (Ast0.Circles(d,Some stms))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | Ast0.Stars(d,_) ->
	(try
	  (match List.assoc (dot_term d) bindings with
	    Ast0.DotsStmtTag(stms) -> Ast0.rewrap e (Ast0.Stars(d,Some stms))
	  | _ -> failwith "unexpected binding")
	with Not_found -> e)
    | _ -> k e in

  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing
    identfn exprfn tyfn paramfn donothing stmtfn donothing

(* --------------------------------------------------------------------- *)

let is_minus (_,_,_,mc,_) =
  match !mc with Ast0.MINUS(cell) -> true | _ -> false

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
	Ast0.MINUS(emc) -> emc := !mc (* ! mc contains no references *)
      |	_ -> failwith "not possible 6");
      e
  | Ast0.CONTEXT(mc) ->
      (match Ast0.get_mcodekind e with
	Ast0.CONTEXT(emc) -> emc := !mc
      |	_ -> failwith "not possible 7");
      e
  | Ast0.MIXED(_) -> failwith "not possible 8"
  | Ast0.PLUS -> failwith "not possible 9"

let whencode_allowed prev_ecount prev_dcount ecount dcount rest =
  (* actually, if ecount or dcount is 0, the flag doesn't matter, because it
     won't be tested *)
  let other_ecount = (* number of edots *)
    List.fold_left (function rest -> function (_,ec,dc) -> ec + rest)
      prev_ecount rest in
  let other_dcount = (* number of dots *)
    List.fold_left (function rest -> function (_,ec,dc) -> dc + rest)
      prev_dcount rest in
  (ecount = 0 or other_ecount = 0, dcount = 0 or other_dcount = 0)

let mkdisj matcher alts instantiater e disj_maker minusify
    rebuild_mcodes printer =
  let call_instantiate bindings alts =
    List.map
      (function (a,_,_) ->
	copy_plus printer minusify e
	  (instantiater bindings (rebuild_mcodes a)))
      alts in
  let rec inner_loop all_alts prev_ecount prev_dcount = function
      [] -> Common.Left (prev_ecount, prev_dcount)
    | ((pattern,ecount,dcount)::rest) ->
	let wc = whencode_allowed prev_ecount prev_dcount ecount dcount rest in
	(match matcher (context_required e) wc pattern e init_env with
	  None ->
	    inner_loop all_alts
	      (prev_ecount + ecount) (prev_dcount + dcount) rest
	| Some bindings ->
	    (match List.concat all_alts with
	      [x] -> Common.Left (prev_ecount, prev_dcount) (* why None??? *)
	    | all_alts ->
		Common.Right (call_instantiate bindings all_alts))) in
  let rec outer_loop prev_ecount prev_dcount = function
      [] -> e (* nothing matched *)
    | (alts::rest) as all_alts ->
	match inner_loop all_alts prev_ecount prev_dcount alts with
	  Common.Left(prev_ecount, prev_dcount) ->
	    outer_loop prev_ecount prev_dcount rest
	| Common.Right res -> disj_maker res in
  outer_loop 0 0 alts

(* no one should ever look at the information stored in these mcodes *)
let disj_starter =
  ("(",Ast0.NONE,Ast0.default_info(),Ast0.context_befaft())

let disj_ender =
  ("(",Ast0.NONE,Ast0.default_info(),Ast0.context_befaft())

let make_disj_expr el =
  Ast0.context_wrap (Ast0.DisjExpr(disj_starter,el,disj_ender))
let make_disj_decl dl =
  Ast0.context_wrap (Ast0.DisjDecl(disj_starter,dl,disj_ender))
let make_disj_stmt sl =
  let dotify x = Ast0.context_wrap (Ast0.DOTS[x]) in
  Ast0.context_wrap (Ast0.Disj(disj_starter,List.map dotify sl,disj_ender))

let transform_expr alts e =
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
		    count_dots.V0.combiner_expression p)
	       | _ -> failwith "invalid alt"))
	  alts in
      mkdisj match_expr alts
	(function b -> (instantiate b).V0.rebuilder_expression) e
	make_disj_expr make_minus.V0.rebuilder_expression
	(rebuild_mcode start_line).V0.rebuilder_expression
	Unparse_ast0.expression
  | _ -> e

let transform_decl alts e =
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
		    count_dots.V0.combiner_declaration p)
	       | _ -> failwith "invalid alt"))
	  alts in
      mkdisj match_decl alts
	(function b -> (instantiate b).V0.rebuilder_declaration) e
	make_disj_decl make_minus.V0.rebuilder_declaration
	(rebuild_mcode start_line).V0.rebuilder_declaration
	Unparse_ast0.declaration
  | _ -> e

let transform_stmt alts e =
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
		    count_dots.V0.combiner_statement p)
	       | _ -> failwith "invalid alt"))
	  alts in
      mkdisj match_statement alts
	(function b -> (instantiate b).V0.rebuilder_statement) e
	make_disj_stmt make_minus.V0.rebuilder_statement
	(rebuild_mcode start_line).V0.rebuilder_statement
	(Unparse_ast0.statement "")
  | _ -> e

(* --------------------------------------------------------------------- *)

let transform (alts : isomorphism) =
  let mcode x = x in
  let exprdotsfn r k e = k e in
  let paramdotsfn r k e = k e in
  let stmtdotsfn r k e = k e in
  let identfn r k e = k e in
  let exprfn r k e = transform_expr alts (k e) in
  let tyfn r k e = k e in
  let paramfn r k e = k e in
  let declfn r k e = transform_decl alts (k e) in
  let stmtfn r k e = transform_stmt alts (k e) in
  let topfn r k e = k e in
  
  let res =
    V0.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      exprdotsfn paramdotsfn stmtdotsfn
      identfn exprfn tyfn paramfn declfn stmtfn topfn in
  res.V0.rebuilder_top_level

(* --------------------------------------------------------------------- *)

(* should be done by functorizing the parser to use wrap or context_wrap *)
let rewrap =
  let mcode (x,a,i,mc) = (x,a,i,Ast0.context_befaft()) in
  let donothing r k e = Ast0.context_wrap(Ast0.unwrap(k e)) in
  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing

let rewrap_anything = function
    Ast0.DotsExprTag(d) ->
      Ast0.DotsExprTag(rewrap.V0.rebuilder_expression_dots d)
  | Ast0.DotsParamTag(d) ->
      Ast0.DotsParamTag(rewrap.V0.rebuilder_parameter_list d)
  | Ast0.DotsStmtTag(d) ->
      Ast0.DotsStmtTag(rewrap.V0.rebuilder_statement_dots d)
  | Ast0.IdentTag(d) -> Ast0.IdentTag(rewrap.V0.rebuilder_ident d)
  | Ast0.ExprTag(d) -> Ast0.ExprTag(rewrap.V0.rebuilder_expression d)
  | Ast0.TypeCTag(d) -> Ast0.TypeCTag(rewrap.V0.rebuilder_typeC d)
  | Ast0.ParamTag(d) -> Ast0.ParamTag(rewrap.V0.rebuilder_parameter d)
  | Ast0.DeclTag(d) -> Ast0.DeclTag(rewrap.V0.rebuilder_declaration d)
  | Ast0.StmtTag(d) -> Ast0.StmtTag(rewrap.V0.rebuilder_statement d)
  | Ast0.TopTag(d) -> Ast0.TopTag(rewrap.V0.rebuilder_top_level d)

(* --------------------------------------------------------------------- *)

let apply_isos isos rule =
  let isos = List.map (List.map (List.map rewrap_anything)) isos in
  Compute_lines.compute_lines
    (List.map
       (function t ->
	 List.fold_left
	   (function t -> function iso -> transform iso t)
	   t isos)
       rule)
