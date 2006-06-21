(* Arities matter for the minus slice, but not for the plus slice. *)

(* + only allowed on code in a nest (in_nest = true).  ? only allowed on
rule_elems, and on subterms if the context is ? also. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)

let warning s = Printf.printf "warning: %s\n" s

let make_opt_unique optfn uniquefn multifn info tgt arity term =
  let term = Ast0.rewrap info term in
  if tgt = arity
  then term
  else (* tgt must be NONE *)
    match arity with
      Ast0.OPT -> Ast0.rewrap info (optfn term)
    | Ast0.UNIQUE -> Ast0.rewrap info (uniquefn term)
    | Ast0.MULTI -> Ast0.rewrap info (multifn term)
    | Ast0.NONE -> failwith "tgt must be NONE"

let all_same multi_allowed opt_allowed tgt line arities =
  let tgt =
    match tgt with
      Ast0.NONE ->
	(match List.hd arities with
	  Ast0.MULTI when not multi_allowed ->
	    failwith "multi only allowed in nests"
	| Ast0.OPT when not opt_allowed ->
	    failwith "opt only allowed for the elements of a statement list"
	| x -> x)
    | _ -> tgt in
  if not(List.for_all (function x -> x = tgt) arities)
  then warning (Printf.sprintf "incompatible arity found on line %d" line);
  tgt

let get_option fn = function
    None -> None
  | Some x -> Some (fn x)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Mcode *)

let mcode2line (_,_,mcodekind) = Ast.get_real_line mcodekind
let mcode2arity (_,arity,_) = arity

let mcode x = x (* nothing to do ... *)

(* --------------------------------------------------------------------- *)
(* Dots *)

let dots fn d =
  Ast0.rewrap d
    (match Ast0.unwrap d with
      Ast0.DOTS(x) -> Ast0.DOTS(List.map fn x)
    | Ast0.CIRCLES(x) -> Ast0.CIRCLES(List.map fn x)
    | Ast0.STARS(x) -> Ast0.STARS(List.map fn x))

let only_dots l =
  not
    (List.exists
       (function x ->
	  match Ast0.unwrap x with
	   Ast0.Circles(_,_) | Ast0.Stars(_,_) -> true
	 | _ -> false)
       l)

let only_circles l =
  not (List.exists
	(function x ->
	  match Ast0.unwrap x with
	    Ast0.Dots(_,_) | Ast0.Stars(_,_) -> true
	  | _ -> false)
	 l)

let only_stars l =
  not (List.exists
	(function x ->
	  match Ast0.unwrap x with
	    Ast0.Dots(_,_) | Ast0.Circles(_,_) -> true
	  | _ -> false)
	 l)

let concat_dots fn d =
  Ast0.rewrap d
    (match Ast0.unwrap d with
      Ast0.DOTS(x) ->
	let l = List.map fn x in
	if only_dots l
	then Ast0.DOTS(l)
	else failwith "inconsistent dots usage"
    | Ast0.CIRCLES(x) ->
	let l = List.map fn x in
	if only_circles l
	then Ast0.CIRCLES(l)
	else failwith "inconsistent dots usage"
    | Ast0.STARS(x) ->
	let l = List.map fn x in
	if only_stars l
	then Ast0.STARS(l)
	else failwith "inconsistent dots usage")

let flat_concat_dots fn d =
  match Ast0.unwrap d with
    Ast0.DOTS(x) -> List.map fn x
  | Ast0.CIRCLES(x) -> List.map fn x
  | Ast0.STARS(x) -> List.map fn x

(* --------------------------------------------------------------------- *)
(* Identifier *)

let make_id =
  make_opt_unique
    (function x -> Ast0.OptIdent x)
    (function x -> Ast0.UniqueIdent x)
    (function x -> Ast0.MultiIdent x)

let ident in_nest opt_allowed tgt i =
  match Ast0.unwrap i with
    Ast0.Id(name) ->
      let arity =
	all_same in_nest opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.Id(name))
  | Ast0.MetaId(name) ->
      let arity =
	all_same in_nest opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.MetaId(name))
  | Ast0.MetaFunc(name) ->
      let arity =
	all_same in_nest opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.MetaFunc(name))
  | Ast0.MetaLocalFunc(name) ->
      let arity =
	all_same in_nest opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.MetaLocalFunc(name))
  | Ast0.OptIdent(_) | Ast0.UniqueIdent(_) | Ast0.MultiIdent(_) ->
      failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Expression *)

let make_exp =
  make_opt_unique
    (function x -> Ast0.OptExp x)
    (function x -> Ast0.UniqueExp x)
    (function x -> Ast0.MultiExp x)

let rec top_expression in_nest opt_allowed tgt expr =
  let exp_same = all_same in_nest opt_allowed tgt in
  match Ast0.unwrap expr with
    Ast0.Ident(id) ->
      Ast0.rewrap expr (Ast0.Ident(ident in_nest opt_allowed tgt id))
  | Ast0.Constant(const) ->
      let arity = exp_same (mcode2line const) [mcode2arity const] in
      let const = mcode const in
      make_exp expr tgt arity (Ast0.Constant(const))
  | Ast0.FunCall(fn,lp,args,rp) ->
      let arity = exp_same (mcode2line lp) [mcode2arity lp;mcode2arity rp] in
      let fn = expression false arity fn in
      let lp = mcode lp in
      let args = dots (expression false arity) args in
      let rp = mcode rp in
      make_exp expr tgt arity (Ast0.FunCall(fn,lp,args,rp))
  | Ast0.Assignment(left,op,right) ->
      let arity = exp_same (mcode2line op) [mcode2arity op] in
      let left = expression false arity left in
      let op = mcode op in
      let right = expression false arity right in
      make_exp expr tgt arity (Ast0.Assignment(left,op,right))
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
      let arity =
	exp_same (mcode2line why) [mcode2arity why; mcode2arity colon] in
      let exp1 = expression false arity exp1 in
      let why = mcode why in
      let exp2 = get_option (expression false arity) exp2 in
      let colon = mcode colon in
      let exp3 = expression false arity exp3 in
      make_exp expr tgt arity (Ast0.CondExpr(exp1,why,exp2,colon,exp3))
  | Ast0.Postfix(exp,op) ->
      let arity = exp_same (mcode2line op) [mcode2arity op] in
      let exp = expression false arity exp in
      let op = mcode op in
      make_exp expr tgt arity (Ast0.Postfix(exp,op))
  | Ast0.Infix(exp,op) ->
      let arity = exp_same (mcode2line op) [mcode2arity op] in
      let exp = expression false arity exp in
      let op = mcode op in
      make_exp expr tgt arity (Ast0.Infix(exp,op))
  | Ast0.Unary(exp,op) ->
      let arity = exp_same (mcode2line op) [mcode2arity op] in
      let exp = expression false arity exp in
      let op = mcode op in
      make_exp expr tgt arity (Ast0.Unary(exp,op))
  | Ast0.Binary(left,op,right) ->
      let arity = exp_same (mcode2line op) [mcode2arity op] in
      let left = expression false arity left in
      let op = mcode op in
      let right = expression false arity right in
      make_exp expr tgt arity (Ast0.Binary(left,op,right))
  | Ast0.Paren(lp,exp,rp) ->
      let arity = exp_same (mcode2line lp) [mcode2arity lp;mcode2arity rp] in
      let lp = mcode lp in
      let exp = expression false arity exp in
      let rp = mcode rp in
      make_exp expr tgt arity (Ast0.Paren(lp,exp,rp))
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
      let arity = exp_same (mcode2line lb) [mcode2arity lb; mcode2arity rb] in
      let exp1 = expression false arity exp1 in
      let lb = mcode lb in
      let exp2 = expression false arity exp2 in
      let rb = mcode rb in
      make_exp expr tgt arity (Ast0.ArrayAccess(exp1,lb,exp2,rb))
  | Ast0.RecordAccess(exp,pt,field) ->
      let arity = exp_same (mcode2line pt) [mcode2arity pt] in
      let exp = expression false arity exp in
      let pt = mcode pt in
      let field = ident false false arity field in
      make_exp expr tgt arity (Ast0.RecordAccess(exp,pt,field))
  | Ast0.RecordPtAccess(exp,ar,field) ->
      let arity = exp_same (mcode2line ar) [mcode2arity ar] in
      let exp = expression false arity exp in
      let ar = mcode ar in
      let field = ident false false arity field in
      make_exp expr tgt arity (Ast0.RecordPtAccess(exp,ar,field))
  | Ast0.Cast(lp,ty,rp,exp) ->
      let arity = exp_same (mcode2line lp) [mcode2arity lp;mcode2arity rp] in
      let lp = mcode lp in
      let ty = fullType arity ty in
      let rp = mcode rp in
      let exp = expression false arity exp in
      make_exp expr tgt arity (Ast0.Cast(lp,ty,rp,exp))
  | Ast0.MetaConst(name,ty)  ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      let ty = get_option (List.map (fullType Ast0.NONE)) ty in
      make_exp expr tgt arity (Ast0.MetaConst(name,ty))
  | Ast0.MetaErr(name)  ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_exp expr tgt arity (Ast0.MetaErr(name))
  | Ast0.MetaExpr(name,ty)  ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      let ty = get_option (List.map (fullType Ast0.NONE)) ty in
      make_exp expr tgt arity (Ast0.MetaExpr(name,ty))
  | Ast0.MetaExprList(name) ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_exp expr tgt arity (Ast0.MetaExprList(name))
  | Ast0.EComma(cm)         ->
      let arity = exp_same (mcode2line cm) [mcode2arity cm] in
      let cm = mcode cm in
      make_exp expr tgt arity (Ast0.EComma(cm))
  | Ast0.DisjExpr(exps) ->
      let res =
	Ast0.DisjExpr(List.map (top_expression in_nest opt_allowed tgt)
			exps) in
      Ast0.rewrap expr res
  | Ast0.NestExpr(exp_dots) ->
      let res =
	Ast0.NestExpr(dots (top_expression true true tgt) exp_dots) in
      Ast0.rewrap expr res
  | Ast0.Edots(dots,whencode) ->
      let arity = exp_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode = get_option (expression false Ast0.NONE) whencode in
      make_exp expr tgt arity (Ast0.Edots(dots,whencode))
  | Ast0.Ecircles(dots,whencode) ->
      let arity = exp_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode = get_option (expression false Ast0.NONE) whencode in
      make_exp expr tgt arity (Ast0.Ecircles(dots,whencode))
  | Ast0.Estars(dots,whencode) ->
      let arity = exp_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode = get_option (expression false Ast0.NONE) whencode in
      make_exp expr tgt arity (Ast0.Estars(dots,whencode))
  | Ast0.OptExp(_) | Ast0.UniqueExp(_) | Ast0.MultiExp(_) ->
      failwith "unexpected code"

and expression in_nest tgt exp =
  top_expression in_nest false tgt exp

(* --------------------------------------------------------------------- *)
(* Types *)

and make_fullType =
  make_opt_unique
    (function x -> Ast0.OptType x)
    (function x -> Ast0.UniqueType x)
    (function x -> Ast0.MultiType x)

and top_fullType tgt opt_allowed ft =
  match Ast0.unwrap ft with
    Ast0.Type(cv,ty) ->
      let cvar =
	match cv with Some cv -> [mcode2arity cv] | None -> [] in
      let cv = get_option mcode cv in
      top_typeC tgt opt_allowed cv cvar ft ty
  | Ast0.OptType(_) | Ast0.UniqueType(_) | Ast0.MultiType(_) ->
      failwith "unexpected code"

and top_typeC tgt opt_allowed cv cvar ft typ =
  match Ast0.unwrap typ with
    Ast0.BaseType(ty,Some sign) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line ty)
	  (cvar@[mcode2arity ty; mcode2arity sign]) in
      let ty = mcode ty in
      let sign = mcode sign in
      make_fullType ft tgt arity
	(Ast0.Type(cv,Ast0.rewrap typ (Ast0.BaseType(ty,Some sign))))
  | Ast0.BaseType(ty,None) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line ty)
	  (cvar@[mcode2arity ty]) in
      let ty = mcode ty in
      make_fullType ft tgt arity
	(Ast0.Type(cv,Ast0.rewrap typ (Ast0.BaseType(ty,None))))
  | Ast0.Pointer(ty,star) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line star)
	  (cvar@[mcode2arity star]) in
      let ty = fullType arity ty in
      let star = mcode star in
      make_fullType ft tgt arity
	(Ast0.Type(cv,Ast0.rewrap typ (Ast0.Pointer(ty,star))))
  | Ast0.Array(ty,lb,size,rb) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line lb)
	  (cvar@[mcode2arity lb;mcode2arity rb]) in
      let ty = fullType arity ty in
      let lb = mcode lb in
      let size = get_option (expression false arity) size in
      let rb = mcode rb in
      make_fullType ft tgt arity
	(Ast0.Type(cv,Ast0.rewrap typ (Ast0.Array(ty,lb,size,rb))))
  | Ast0.StructUnionName(name,kind) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line name)
	  (cvar@[mcode2arity name;mcode2arity kind]) in
      let name = mcode name in
      let kind = mcode kind in
      make_fullType ft tgt arity
	(Ast0.Type(cv,Ast0.rewrap typ (Ast0.StructUnionName(name,kind))))
  | Ast0.TypeName(name) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line name)
	  (cvar@[mcode2arity name]) in
      let name = mcode name in
      make_fullType ft tgt arity
	(Ast0.Type(cv,Ast0.rewrap typ (Ast0.TypeName(name))))
  | Ast0.MetaType(name) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line name)
	  (cvar@[mcode2arity name]) in
      let name = mcode name in
      make_fullType ft tgt arity
	(Ast0.Type(cv,Ast0.rewrap typ (Ast0.MetaType(name))))

and fullType tgt ty = top_fullType tgt false ty

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

let make_decl =
  make_opt_unique
    (function x -> Ast0.OptDecl x)
    (function x -> Ast0.UniqueDecl x)
    (function x -> Ast0.MultiDecl x)

let declaration in_nest tgt decl =
  match Ast0.unwrap decl with
    Ast0.Init(ty,id,eq,exp,sem) ->
      let arity =
	all_same in_nest true tgt (mcode2line eq)
	  [mcode2arity eq;mcode2arity sem] in
      let ty = fullType arity ty in
      let id = ident false false arity id in
      let eq = mcode eq in
      let exp = expression false arity exp in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.Init(ty,id,eq,exp,sem))
  | Ast0.UnInit(ty,id,sem) ->
      let arity =
	all_same in_nest true tgt (mcode2line sem) [mcode2arity sem] in
      let ty = fullType arity ty in
      let id = ident false false arity id in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.UnInit(ty,id,sem))
  | Ast0.OptDecl(_) | Ast0.UniqueDecl(_) | Ast0.MultiDecl(_) ->
      failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Parameter *)

let make_param =
  make_opt_unique
    (function x -> Ast0.OptParam x)
    (function x -> Ast0.UniqueParam x)
    (function x -> failwith "multi not allowed for parameters")

let parameterTypeDef tgt param =
  let param_same = all_same false true tgt in
  match Ast0.unwrap param with
    Ast0.VoidParam(ty) -> Ast0.rewrap param (Ast0.VoidParam(fullType tgt ty))
  | Ast0.Param(id,ty) ->
      let id = ident false true tgt id in
      let ty = top_fullType tgt true ty in
      Ast0.rewrap param 
	(match (Ast0.unwrap id,Ast0.unwrap ty) with
	  (Ast0.OptIdent(id),Ast0.OptType(ty)) ->
	    Ast0.OptParam(Ast0.rewrap param (Ast0.Param(id,ty)))
	| (Ast0.UniqueIdent(id),Ast0.UniqueType(ty)) ->
	    Ast0.UniqueParam(Ast0.rewrap param (Ast0.Param(id,ty)))
	| (Ast0.OptIdent(id),_) ->
	    failwith "arity mismatch in param declaration"
	| (_,Ast0.OptType(ty)) ->
	    failwith "arity mismatch in param declaration"
	| _ -> Ast0.Param(id,ty))
  | Ast0.MetaParam(name) ->
      let arity = param_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_param param tgt arity (Ast0.MetaParam(name))
  | Ast0.MetaParamList(name) ->
      let arity = param_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_param param tgt arity (Ast0.MetaParamList(name))
  | Ast0.PComma(cm) ->
      let arity = param_same (mcode2line cm) [mcode2arity cm] in
      let cm = mcode cm in
      make_param param tgt arity (Ast0.PComma(cm))
  | Ast0.Pdots(dots) ->
      let arity = param_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      make_param param tgt arity (Ast0.Pdots(dots))
  | Ast0.Pcircles(dots) ->
      let arity = param_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      make_param param tgt arity (Ast0.Pcircles(dots))
  | Ast0.OptParam(_) | Ast0.UniqueParam(_) ->
      failwith "unexpected code"

let parameter_list tgt = dots (parameterTypeDef tgt)

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let make_rule_elem =
  make_opt_unique
    (function x -> Ast0.OptStm x)
    (function x -> Ast0.UniqueStm x)
    (function x -> Ast0.MultiStm x)

let rec statement in_nest tgt stm =
  let stm_same = all_same in_nest true tgt in
  match Ast0.unwrap stm with
    Ast0.Decl(decl) ->
      Ast0.rewrap stm (Ast0.Decl(declaration in_nest tgt decl))
  | Ast0.Seq(lbrace,body,rbrace) -> 
      let arity =
	stm_same (mcode2line lbrace)
	  [mcode2arity lbrace; mcode2arity rbrace] in
      let lbrace = mcode lbrace in
      let body = dots (statement false arity) body in
      let rbrace = mcode rbrace in
      make_rule_elem stm tgt arity (Ast0.Seq(lbrace,body,rbrace))
  | Ast0.ExprStatement(exp,sem) ->
      let arity = stm_same (mcode2line sem) [mcode2arity sem] in
      let exp = expression false arity exp in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.ExprStatement(exp,sem))
  | Ast0.IfThen(iff,lp,exp,rp,branch) ->
      let arity =
	stm_same (mcode2line iff) (List.map mcode2arity [iff;lp;rp]) in
      let iff = mcode iff in
      let lp = mcode lp in
      let exp = expression false arity exp in
      let rp = mcode rp in
      let branch = statement false arity branch in
      make_rule_elem stm tgt arity (Ast0.IfThen(iff,lp,exp,rp,branch))
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
      let arity =
	stm_same (mcode2line iff) (List.map mcode2arity [iff;lp;rp;els]) in
      let iff = mcode iff in
      let lp = mcode lp in
      let exp = expression false arity exp in
      let rp = mcode rp in
      let branch1 = statement false arity branch1 in
      let els = mcode els in
      let branch2 = statement false arity branch2 in
      make_rule_elem stm tgt arity
	(Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2))
  | Ast0.While(wh,lp,exp,rp,body) ->
      let arity =
	stm_same (mcode2line wh)
	  (List.map mcode2arity [wh;lp;rp]) in
      let wh = mcode wh in
      let lp = mcode lp in
      let exp = expression false arity exp in
      let rp = mcode rp in
      let body = statement false arity body in
      make_rule_elem stm tgt arity (Ast0.While(wh,lp,exp,rp,body))
  | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
      let arity =
	stm_same (mcode2line wh) (List.map mcode2arity [d;wh;lp;rp;sem]) in
      let d = mcode d in
      let body = statement false arity body in
      let wh = mcode wh in
      let lp = mcode lp in
      let exp = expression false arity exp in
      let rp = mcode rp in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.Do(d,body,wh,lp,exp,rp,sem))
  | Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body) ->
      let arity =
	stm_same (mcode2line fr) (List.map mcode2arity [fr;lp;sem1;sem2;rp]) in
      let fr = mcode fr in
      let lp = mcode lp in
      let exp1 = get_option (expression false arity) exp1 in
      let sem1 = mcode sem1 in
      let exp2 = get_option (expression false arity) exp2 in
      let sem2= mcode sem2 in
      let exp3 = get_option (expression false arity) exp3 in
      let rp = mcode rp in
      let body = statement false arity body in
      make_rule_elem stm tgt arity
	(Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body))
  | Ast0.Return(ret,sem) ->
      let arity = stm_same (mcode2line ret) (List.map mcode2arity [ret;sem]) in
      let ret = mcode ret in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.Return(ret,sem))
  | Ast0.ReturnExpr(ret,exp,sem) ->
      let arity = stm_same (mcode2line ret) (List.map mcode2arity [ret;sem]) in
      let ret = mcode ret in
      let exp = expression false arity exp in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.ReturnExpr(ret,exp,sem))
  | Ast0.MetaStmt(name) ->
      let arity = stm_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_rule_elem stm tgt arity (Ast0.MetaStmt(name))
  | Ast0.MetaStmtList(name) ->
      let arity = stm_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_rule_elem stm tgt arity (Ast0.MetaStmtList(name))
  | Ast0.Exp(exp) ->
      Ast0.rewrap stm (Ast0.Exp(top_expression in_nest true tgt exp))
  | Ast0.Disj(rule_elem_dots_list) ->
      Ast0.rewrap stm
	(Ast0.Disj(List.map
		     (function x -> concat_dots (statement in_nest tgt) x)
		     rule_elem_dots_list))
  | Ast0.Nest(rule_elem_dots) ->
      Ast0.rewrap stm
	(Ast0.Nest(concat_dots (statement true tgt) rule_elem_dots))
  | Ast0.Dots(dots,whencode)    ->
      let arity = stm_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode =
	get_option (concat_dots (statement false Ast0.NONE)) whencode in
      make_rule_elem stm tgt arity (Ast0.Dots(dots,whencode))
  | Ast0.Circles(dots,whencode) ->
      let arity = stm_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode =
	get_option (concat_dots (statement false Ast0.NONE)) whencode in
      make_rule_elem stm tgt arity (Ast0.Circles(dots,whencode))
  | Ast0.Stars(dots,whencode)   ->
      let arity = stm_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode =
	get_option (concat_dots (statement false Ast0.NONE)) whencode in
      make_rule_elem stm tgt arity (Ast0.Stars(dots,whencode))
  | Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace) ->
      let arity =
	all_same false true tgt (mcode2line lp)
	  ((match stg with None -> [] | Some x -> [mcode2arity x]) @
	   (List.map mcode2arity [lp;rp;lbrace;rbrace])) in
      let stg =
	match stg with
	  None -> None
	| Some x -> Some (mcode x) in
      let name = ident false false arity name in
      let lp = mcode lp in
      let params = parameter_list arity params in
      let rp = mcode rp in
      let lbrace = mcode lbrace in
      let body = dots (statement false arity) body in
      let rbrace = mcode rbrace in
      make_rule_elem stm tgt arity
	(Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace))
  | Ast0.OptStm(_) | Ast0.UniqueStm(_) | Ast0.MultiStm(_) ->
      failwith "unexpected code"	
(* --------------------------------------------------------------------- *)
(* Function declaration *)
(* Haven't thought much about arity here... *)

let top_level tgt t =
  Ast0.rewrap t
    (match Ast0.unwrap t with
      Ast0.DECL(decl) -> Ast0.DECL(declaration false Ast0.NONE decl)
    | Ast0.INCLUDE(inc,s) ->
	if mcode2arity inc = Ast0.NONE && mcode2arity s = Ast0.NONE
	then Ast0.INCLUDE(mcode inc,mcode s)
	else failwith "unexpected arity for include"
    | Ast0.FILEINFO(old_file,new_file) -> 
	if mcode2arity old_file = Ast0.NONE && mcode2arity new_file = Ast0.NONE
	then Ast0.FILEINFO(mcode old_file,mcode new_file)
	else failwith "unexpected arity for include"
    | Ast0.FUNCTION(stmt) ->
	Ast0.FUNCTION(statement false tgt stmt)
    | Ast0.CODE(rule_elem_dots) ->
	Ast0.CODE(concat_dots (statement false tgt) rule_elem_dots)
    | Ast0.ERRORWORDS(exps) ->
	Ast0.ERRORWORDS(List.map (top_expression false false Ast0.NONE) exps)
    | Ast0.OTHER(_) -> failwith "eliminated by top_level")

let rule tgt = List.map (top_level tgt)

(* --------------------------------------------------------------------- *)
(* Entry points *)

let minus_arity code =
  rule Ast0.NONE code
