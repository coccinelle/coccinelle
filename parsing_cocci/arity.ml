(* Arities matter for the minus slice, but not for the plus slice. *)

(* + only allowed on code in a nest (in_nest = true).  ? only allowed on
rule_elems, and on subterms if the context is ? also. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)

let warning s = Printf.printf "warning: %s\n" s

let fail w str =
  failwith
    (Printf.sprintf "cocci line %d: %s" ((Ast0.get_info w).Ast0.line_start)
       str)

let make_opt_unique optfn uniquefn multifn info tgt arity term =
  let term = Ast0.rewrap info term in
  if tgt = arity
  then term
  else (* tgt must be NONE *)
    match arity with
      Ast0.OPT -> Ast0.copywrap info (optfn term)
    | Ast0.UNIQUE -> Ast0.copywrap info (uniquefn term)
    | Ast0.MULTI -> Ast0.copywrap info (multifn term)
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

let anyopt l fn = List.exists (function w -> fn(Ast0.unwrap w)) l

let allopt l fn =
  let rec loop = function
      [] -> []
    | x::xs ->
	match fn (Ast0.unwrap x) with
	  Some x -> x :: (loop xs)
	| None -> [] in
  let res = loop l in
  if List.length res = List.length l then Some res else None

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Mcode *)

let mcode2line (_,_,info,_) = info.Ast0.line_start
let mcode2arity (_,arity,_,_) = arity

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
	else fail d "inconsistent dots usage"
    | Ast0.CIRCLES(x) ->
	let l = List.map fn x in
	if only_circles l
	then Ast0.CIRCLES(l)
	else fail d "inconsistent dots usage"
    | Ast0.STARS(x) ->
	let l = List.map fn x in
	if only_stars l
	then Ast0.STARS(l)
	else fail d "inconsistent dots usage")

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
  | Ast0.MetaId(name,pure) ->
      let arity =
	all_same in_nest opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.MetaId(name,pure))
  | Ast0.MetaFunc(name,pure) ->
      let arity =
	all_same in_nest opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.MetaFunc(name,pure))
  | Ast0.MetaLocalFunc(name,pure) ->
      let arity =
	all_same in_nest opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.MetaLocalFunc(name,pure))
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
      let new_id = ident in_nest opt_allowed tgt id in
      Ast0.rewrap expr
	(match Ast0.unwrap new_id with
	  Ast0.OptIdent(id) ->
	    Ast0.OptExp(Ast0.rewrap expr (Ast0.Ident(id)))
	| Ast0.UniqueIdent(id) ->
	    Ast0.UniqueExp(Ast0.rewrap expr (Ast0.Ident(id)))
	| Ast0.MultiIdent(id) ->
	    Ast0.MultiExp(Ast0.rewrap expr (Ast0.Ident(id)))
	| _ -> Ast0.Ident(new_id))
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
      let ty = typeC arity ty in
      let rp = mcode rp in
      let exp = expression false arity exp in
      make_exp expr tgt arity (Ast0.Cast(lp,ty,rp,exp))
  | Ast0.SizeOfExpr(szf,exp) ->
      let arity = exp_same (mcode2line szf) [mcode2arity szf] in
      let szf = mcode szf in
      let exp = expression false arity exp in
      make_exp expr tgt arity (Ast0.SizeOfExpr(szf,exp))
  | Ast0.SizeOfType(szf,lp,ty,rp) ->
      let arity =
	exp_same (mcode2line szf) (List.map mcode2arity [szf;lp;rp]) in
      let szf = mcode szf in
      let lp = mcode lp in
      let ty = typeC arity ty in
      let rp = mcode rp in
      make_exp expr tgt arity (Ast0.SizeOfType(szf,lp,ty,rp))
  | Ast0.TypeExp(ty) -> Ast0.rewrap expr (Ast0.TypeExp(typeC tgt ty))
  | Ast0.MetaConst(name,ty,pure)  ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_exp expr tgt arity (Ast0.MetaConst(name,ty,pure))
  | Ast0.MetaErr(name,pure)  ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_exp expr tgt arity (Ast0.MetaErr(name,pure))
  | Ast0.MetaExpr(name,ty,pure)  ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_exp expr tgt arity (Ast0.MetaExpr(name,ty,pure))
  | Ast0.MetaExprList(name,pure) ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_exp expr tgt arity (Ast0.MetaExprList(name,pure))
  | Ast0.EComma(cm)         ->
      let arity = exp_same (mcode2line cm) [mcode2arity cm] in
      let cm = mcode cm in
      make_exp expr tgt arity (Ast0.EComma(cm))
  | Ast0.DisjExpr(starter,exps,mids,ender) ->
      let exps = List.map (top_expression in_nest opt_allowed tgt) exps in
      (match List.rev exps with
	_::xs ->
	  if anyopt xs (function Ast0.OptExp(_) -> true | _ -> false)
	  then fail expr "opt only allowed in the last disjunct"
      |	_ -> ());
      Ast0.rewrap expr
	(match allopt exps (function Ast0.MultiExp(e) -> Some e | _ -> None)
	with
	  Some stripped ->
	    Ast0.MultiExp
	      (Ast0.rewrap expr(Ast0.DisjExpr(starter,stripped,mids,ender)))
	| None -> Ast0.DisjExpr(starter,exps,mids,ender))
  | Ast0.NestExpr(starter,exp_dots,ender,whencode) ->
      let res =
	Ast0.NestExpr(starter,dots (top_expression true true tgt) exp_dots,
		      ender,whencode) in
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

and make_typeC =
  make_opt_unique
    (function x -> Ast0.OptType x)
    (function x -> Ast0.UniqueType x)
    (function x -> Ast0.MultiType x)

and top_typeC tgt opt_allowed typ =
  match Ast0.unwrap typ with
    Ast0.ConstVol(cv,ty) ->
      let arity = all_same false opt_allowed tgt (mcode2line cv)
	  [mcode2arity cv] in
      let cv = mcode cv in
      let ty = typeC arity ty in
      make_typeC typ tgt arity (Ast0.ConstVol(cv,ty))
  | Ast0.BaseType(ty,Some sign) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line ty)
	  [mcode2arity ty; mcode2arity sign] in
      let ty = mcode ty in
      let sign = mcode sign in
      make_typeC typ tgt arity (Ast0.BaseType(ty,Some sign))
  | Ast0.BaseType(ty,None) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line ty) [mcode2arity ty] in
      let ty = mcode ty in
      make_typeC typ tgt arity (Ast0.BaseType(ty,None))
  | Ast0.ImplicitInt(sign) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line sign) [mcode2arity sign] in
      let sign = mcode sign in
      make_typeC typ tgt arity (Ast0.ImplicitInt(sign))
  | Ast0.Pointer(ty,star) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line star) [mcode2arity star] in
      let ty = typeC arity ty in
      let star = mcode star in
      make_typeC typ tgt arity (Ast0.Pointer(ty,star))
  | Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line lp1)
	  (List.map mcode2arity [lp1;star;rp1;lp2;rp2]) in
      let ty = typeC arity ty in
      let params = parameter_list tgt params in
      make_typeC typ tgt arity
	(Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2))
  | Ast0.FunctionType(ty,lp1,params,rp1) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line lp1)
	  (List.map mcode2arity [lp1;rp1]) in
      let ty = get_option (typeC arity) ty in
      let params = parameter_list tgt params in
      make_typeC typ tgt arity (Ast0.FunctionType(ty,lp1,params,rp1))
  | Ast0.Array(ty,lb,size,rb) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line lb)
	  [mcode2arity lb;mcode2arity rb] in
      let ty = typeC arity ty in
      let lb = mcode lb in
      let size = get_option (expression false arity) size in
      let rb = mcode rb in
      make_typeC typ tgt arity (Ast0.Array(ty,lb,size,rb))
  | Ast0.StructUnionName(kind,name) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line kind)
	  [mcode2arity kind] in
      let kind = mcode kind in
      let name = ident false false arity name in
      make_typeC typ tgt arity (Ast0.StructUnionName(kind,name))
  | Ast0.StructUnionDef(ty,lb,decls,rb) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line lb)
	  (List.map mcode2arity [lb;rb]) in
      let ty = typeC arity ty in
      let lb = mcode lb in
      let decls = dots (declaration false tgt) decls in
      let rb = mcode rb in
      make_typeC typ tgt arity (Ast0.StructUnionDef(ty,lb,decls,rb))
  | Ast0.TypeName(name) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_typeC typ tgt arity (Ast0.TypeName(name))
  | Ast0.MetaType(name,pure) ->
      let arity =
	all_same false opt_allowed tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_typeC typ tgt arity (Ast0.MetaType(name,pure))
  | Ast0.DisjType(starter,types,mids,ender) ->
      let types = List.map (typeC tgt) types in
      (match List.rev types with
	_::xs ->
	  if anyopt xs (function Ast0.OptType(_) -> true | _ -> false)
	  then fail typ "opt only allowed in the last disjunct"
      |	_ -> ());
      let res = Ast0.DisjType(starter,types,mids,ender) in
      Ast0.rewrap typ res
  | Ast0.OptType(_) | Ast0.UniqueType(_) | Ast0.MultiType(_) ->
      failwith "unexpected code"

and typeC tgt ty = top_typeC tgt false ty

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and make_decl =
  make_opt_unique
    (function x -> Ast0.OptDecl x)
    (function x -> Ast0.UniqueDecl x)
    (function x -> Ast0.MultiDecl x)

and declaration in_nest tgt decl =
  match Ast0.unwrap decl with
    Ast0.Init(stg,ty,id,eq,exp,sem) ->
      let arity =
	all_same in_nest true tgt (mcode2line eq)
	  ((match stg with None -> [] | Some x -> [mcode2arity x]) @
	   (List.map mcode2arity [eq;sem])) in
      let stg = get_option mcode stg in
      let ty = typeC arity ty in
      let id = ident false false arity id in
      let eq = mcode eq in
      let exp = initialiser arity exp in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.Init(stg,ty,id,eq,exp,sem))
  | Ast0.UnInit(stg,ty,id,sem) ->
      let arity =
	all_same in_nest true tgt (mcode2line sem)
	  ((match stg with None -> [] | Some x -> [mcode2arity x]) @
	   [mcode2arity sem]) in
      let stg = get_option mcode stg in
      let ty = typeC arity ty in
      let id = ident false false arity id in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.UnInit(stg,ty,id,sem))
  | Ast0.MacroDecl(name,lp,args,rp,sem) ->
      let arity =
	all_same in_nest true tgt
	  (mcode2line lp) (List.map mcode2arity [name;lp;rp;sem]) in
      let name = mcode name in
      let lp = mcode lp in
      let args = dots (expression false arity) args in
      let rp = mcode rp in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.MacroDecl(name,lp,args,rp,sem))
  | Ast0.TyDecl(ty,sem) ->
      let arity =
	all_same in_nest true tgt (mcode2line sem) [mcode2arity sem] in
      let ty = typeC arity ty in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.TyDecl(ty,sem))
  | Ast0.DisjDecl(starter,decls,mids,ender) ->
      let decls = List.map (declaration in_nest tgt) decls in
      (match List.rev decls with
	_::xs ->
	  if anyopt xs (function Ast0.OptDecl(_) -> true | _ -> false)
	  then fail decl "opt only allowed in the last disjunct"
      |	_ -> ());
      let res = Ast0.DisjDecl(starter,decls,mids,ender) in
      Ast0.rewrap decl res
  | Ast0.Ddots(dots,whencode) ->
      let arity =
	all_same false true tgt (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode = get_option (declaration false Ast0.NONE) whencode in
      make_decl decl tgt arity (Ast0.Ddots(dots,whencode))
  | Ast0.OptDecl(_) | Ast0.UniqueDecl(_) | Ast0.MultiDecl(_) ->
      failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Initializer *)

and make_init =
  make_opt_unique
    (function x -> Ast0.OptIni x)
    (function x -> Ast0.UniqueIni x)
    (function x -> Ast0.MultiIni x)

and initialiser tgt i =
  let init_same = all_same false true tgt in
  match Ast0.unwrap i with
    Ast0.InitExpr(exp) ->
      Ast0.rewrap i (Ast0.InitExpr(expression false tgt exp))
  | Ast0.InitList(lb,initlist,rb) ->
      let arity = init_same (mcode2line lb) [mcode2arity lb; mcode2arity rb] in
      let lb = mcode lb in
      let initlist = dots (initialiser arity) initlist in
      let rb = mcode rb in
      make_init i tgt arity (Ast0.InitList(lb,initlist,rb))
  | Ast0.InitGccDotName(dot,name,eq,ini) ->
      let arity =
	init_same (mcode2line dot) [mcode2arity dot; mcode2arity eq] in
      let dot = mcode dot in
      let name = ident false true arity name in
      let eq = mcode eq in
      let ini = initialiser arity ini in
      make_init i tgt arity (Ast0.InitGccDotName(dot,name,eq,ini))
  | Ast0.InitGccName(name,eq,ini) ->
      let arity = init_same (mcode2line eq) [mcode2arity eq] in
      let name = ident false true arity name in
      let eq = mcode eq in
      let ini = initialiser arity ini in
      make_init i tgt arity (Ast0.InitGccName(name,eq,ini))
  | Ast0.InitGccIndex(lb,exp,rb,eq,ini) ->
      let arity =
	init_same (mcode2line lb)
	  [mcode2arity lb; mcode2arity rb; mcode2arity eq] in
      let lb = mcode lb in
      let exp = expression false arity exp in
      let rb = mcode rb in
      let eq = mcode eq in
      let ini = initialiser arity ini in
      make_init i tgt arity (Ast0.InitGccIndex(lb,exp,rb,eq,ini))
  | Ast0.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini) ->
      let arity =
	init_same (mcode2line lb)
	  [mcode2arity lb; mcode2arity dots; mcode2arity rb; mcode2arity eq] in
      let lb = mcode lb in
      let exp1 = expression false arity exp1 in
      let dots = mcode dots in
      let exp2 = expression false arity exp2 in
      let rb = mcode rb in
      let eq = mcode eq in
      let ini = initialiser arity ini in
      make_init i tgt arity
	(Ast0.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini))
  | Ast0.IComma(cm) ->
      let arity = init_same (mcode2line cm) [mcode2arity cm] in
      let cm = mcode cm in
      make_init i tgt arity (Ast0.IComma(cm))
  | Ast0.Idots(dots,whencode) ->
      let arity = init_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode = get_option (initialiser Ast0.NONE) whencode in
      make_init i tgt arity (Ast0.Idots(dots,whencode))
  | Ast0.OptIni(_) | Ast0.UniqueIni(_) | Ast0.MultiIni(_) ->
      failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Parameter *)

and make_param =
  make_opt_unique
    (function x -> Ast0.OptParam x)
    (function x -> Ast0.UniqueParam x)
    (function x -> failwith "multi not allowed for parameters")

and parameterTypeDef tgt param =
  let param_same = all_same false true tgt in
  match Ast0.unwrap param with
    Ast0.VoidParam(ty) -> Ast0.rewrap param (Ast0.VoidParam(typeC tgt ty))
  | Ast0.Param(ty,Some id) ->
      let ty = top_typeC tgt true ty in
      let id = ident false true tgt id in
      Ast0.rewrap param 
	(match (Ast0.unwrap ty,Ast0.unwrap id) with
	  (Ast0.OptType(ty),Ast0.OptIdent(id)) ->
	    Ast0.OptParam(Ast0.rewrap param (Ast0.Param(ty,Some id)))
	| (Ast0.UniqueType(ty),Ast0.UniqueIdent(id)) ->
	    Ast0.UniqueParam(Ast0.rewrap param (Ast0.Param(ty,Some id)))
	| (Ast0.OptType(ty),_) ->
	    fail param "arity mismatch in param declaration"
	| (_,Ast0.OptIdent(id)) ->
	    fail param "arity mismatch in param declaration"
	| _ -> Ast0.Param(ty,Some id))
  | Ast0.Param(ty,None) ->
      let ty = top_typeC tgt true ty in
      Ast0.rewrap param 
	(match Ast0.unwrap ty with
	  Ast0.OptType(ty) ->
	    Ast0.OptParam(Ast0.rewrap param (Ast0.Param(ty,None)))
	| Ast0.UniqueType(ty) ->
	    Ast0.UniqueParam(Ast0.rewrap param (Ast0.Param(ty,None)))
	| _ -> Ast0.Param(ty,None))
  | Ast0.MetaParam(name,pure) ->
      let arity = param_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_param param tgt arity (Ast0.MetaParam(name,pure))
  | Ast0.MetaParamList(name,pure) ->
      let arity = param_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_param param tgt arity (Ast0.MetaParamList(name,pure))
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

and parameter_list tgt = dots (parameterTypeDef tgt)

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
    Ast0.Decl(bef,decl) ->
      let new_decl = declaration in_nest tgt decl in
      Ast0.rewrap stm 
	(match Ast0.unwrap new_decl with
	  Ast0.OptDecl(decl) ->
	    Ast0.OptStm(Ast0.rewrap stm (Ast0.Decl(bef,decl)))
	| Ast0.UniqueDecl(decl) ->
	    Ast0.UniqueStm(Ast0.rewrap stm (Ast0.Decl(bef,decl)))
	| Ast0.MultiDecl(decl) ->
	    Ast0.MultiStm(Ast0.rewrap stm (Ast0.Decl(bef,decl)))
	| _ -> Ast0.Decl(bef,new_decl))
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
  | Ast0.IfThen(iff,lp,exp,rp,branch,aft) ->
      let arity =
	stm_same (mcode2line iff) (List.map mcode2arity [iff;lp;rp]) in
      let iff = mcode iff in
      let lp = mcode lp in
      let exp = expression false arity exp in
      let rp = mcode rp in
      let branch = statement false arity branch in
      make_rule_elem stm tgt arity (Ast0.IfThen(iff,lp,exp,rp,branch,aft))
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) ->
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
	(Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft))
  | Ast0.While(wh,lp,exp,rp,body,aft) ->
      let arity =
	stm_same (mcode2line wh)
	  (List.map mcode2arity [wh;lp;rp]) in
      let wh = mcode wh in
      let lp = mcode lp in
      let exp = expression false arity exp in
      let rp = mcode rp in
      let body = statement false arity body in
      make_rule_elem stm tgt arity (Ast0.While(wh,lp,exp,rp,body,aft))
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
  | Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body,aft) ->
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
	(Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body,aft))
  | Ast0.Switch(switch,lp,exp,rp,lb,cases,rb) ->
      let arity =
	stm_same (mcode2line switch)
	  (List.map mcode2arity [switch;lp;rp;lb;rb]) in
      let switch = mcode switch in
      let lp = mcode lp in
      let exp = expression false arity exp in
      let rp = mcode rp in
      let lb = mcode lb in
      let cases = dots (case_line arity) cases in
      let rb = mcode rb in
      make_rule_elem stm tgt arity
	(Ast0.Switch(switch,lp,exp,rp,lb,cases,rb))
  | Ast0.Break(br,sem) ->
      let arity = stm_same (mcode2line br) (List.map mcode2arity [br;sem]) in
      let br = mcode br in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.Break(br,sem))
  | Ast0.Continue(cont,sem) ->
      let arity =
	stm_same (mcode2line cont) (List.map mcode2arity [cont;sem]) in
      let cont = mcode cont in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.Continue(cont,sem))
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
  | Ast0.MetaStmt(name,pure) ->
      let arity = stm_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_rule_elem stm tgt arity (Ast0.MetaStmt(name,pure))
  | Ast0.MetaStmtList(name,pure) ->
      let arity = stm_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_rule_elem stm tgt arity (Ast0.MetaStmtList(name,pure))
  | Ast0.Exp(exp) ->
      let new_exp = top_expression in_nest true tgt exp in
      Ast0.rewrap stm 
	(match Ast0.unwrap new_exp with
	  Ast0.OptExp(exp) ->
	    Ast0.OptStm(Ast0.rewrap stm (Ast0.Exp(exp)))
	| Ast0.UniqueExp(exp) ->
	    Ast0.UniqueStm(Ast0.rewrap stm (Ast0.Exp(exp)))
	| Ast0.MultiExp(exp) ->
	    Ast0.MultiStm(Ast0.rewrap stm (Ast0.Exp(exp)))
	| _ -> Ast0.Exp(new_exp))
  | Ast0.Ty(ty) ->
      let new_ty = typeC tgt ty in (* opt makes no sense alone at top level *)
      Ast0.rewrap stm 
	(match Ast0.unwrap new_ty with
	  Ast0.OptType(ty) ->
	    Ast0.OptStm(Ast0.rewrap stm (Ast0.Ty(ty)))
	| Ast0.UniqueType(ty) ->
	    Ast0.UniqueStm(Ast0.rewrap stm (Ast0.Ty(ty)))
	| Ast0.MultiType(ty) ->
	    Ast0.MultiStm(Ast0.rewrap stm (Ast0.Ty(ty)))
	| _ -> Ast0.Ty(new_ty))
  | Ast0.Disj(starter,rule_elem_dots_list,mids,ender) ->
      let stms =
	List.map (function x -> concat_dots (statement in_nest tgt) x)
	  rule_elem_dots_list in
      (try
	let unoptd =
	  List.map
	    (function x ->
	      let rebuild =
		List.map
		  (function x ->
		    match Ast0.unwrap x with
		      Ast0.OptStm(x) -> x
		    | Ast0.Dots(_,_) -> x
		    | _ -> failwith "") in
	      match Ast0.unwrap x with
		Ast0.DOTS(l) -> Ast0.rewrap x (Ast0.DOTS(rebuild l))
	      | Ast0.CIRCLES(l) -> Ast0.rewrap x (Ast0.CIRCLES(rebuild l))
	      | Ast0.STARS(l) -> Ast0.rewrap x (Ast0.STARS(rebuild l)))
	    stms in
	make_rule_elem stm tgt Ast0.OPT (Ast0.Disj(starter,unoptd,mids,ender))
      with Failure _ -> Ast0.rewrap stm (Ast0.Disj(starter,stms,mids,ender)))
  | Ast0.Nest(starter,rule_elem_dots,ender,whencode) ->
      let new_rule_elem_dots =
	concat_dots (statement true tgt) rule_elem_dots in
      Ast0.rewrap stm (Ast0.Nest(starter,new_rule_elem_dots,ender,whencode))
  | Ast0.Dots(dots,whn)    ->
      let arity = stm_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whn =
	whencode (concat_dots (statement false Ast0.NONE))
	  (statement false Ast0.NONE) whn in
      make_rule_elem stm tgt arity (Ast0.Dots(dots,whn))
  | Ast0.Circles(dots,whn) ->
      let arity = stm_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whn =
	whencode (concat_dots (statement false Ast0.NONE))
	  (statement false Ast0.NONE) whn in
      make_rule_elem stm tgt arity (Ast0.Circles(dots,whn))
  | Ast0.Stars(dots,whn)   ->
      let arity = stm_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whn =
	whencode (concat_dots (statement false Ast0.NONE))
	  (statement false Ast0.NONE) whn in
      make_rule_elem stm tgt arity (Ast0.Stars(dots,whn))
  | Ast0.FunDecl(bef,stg,ty,name,lp,params,rp,lbrace,body,rbrace) ->
      let arity =
	all_same false true tgt (mcode2line lp)
	  ((match stg with None -> [] | Some x -> [mcode2arity x]) @
	   (List.map mcode2arity [lp;rp;lbrace;rbrace])) in
      let stg = get_option mcode stg in
      let ty = get_option (typeC arity) ty in
      let name = ident false false arity name in
      let lp = mcode lp in
      let params = parameter_list arity params in
      let rp = mcode rp in
      let lbrace = mcode lbrace in
      let body = dots (statement false arity) body in
      let rbrace = mcode rbrace in
      make_rule_elem stm tgt arity
	(Ast0.FunDecl(bef,stg,ty,name,lp,params,rp,lbrace,body,rbrace))
  | Ast0.Include(inc,s) -> 
      let arity =
	all_same true true tgt (mcode2line inc)
	  (List.map mcode2arity [inc;s]) in
      let inc = mcode inc in
      let s = mcode s in
      make_rule_elem stm tgt arity (Ast0.Include(inc,s))
  | Ast0.Define(def,id,params,body) ->
      let arity =
	all_same true true tgt (mcode2line def) [mcode2arity def] in
      let def = mcode def in
      let id = ident false false arity id in
      let params = get_option mcode params in
      let body = define_body arity body in
      make_rule_elem stm tgt arity (Ast0.Define(def,id,params,body))
  | Ast0.OptStm(_) | Ast0.UniqueStm(_) | Ast0.MultiStm(_) ->
      failwith "unexpected code"	

and whencode notfn alwaysfn = function
    Ast0.NoWhen -> Ast0.NoWhen
  | Ast0.WhenNot a -> Ast0.WhenNot (notfn a)
  | Ast0.WhenAlways a -> Ast0.WhenAlways (alwaysfn a)

and make_case_line =
  make_opt_unique
    (function x -> Ast0.OptCase x)
    (function x -> failwith "unique not allowed for case_line")
    (function x -> failwith "multi not allowed for case_line")

and case_line tgt c =
  match Ast0.unwrap c with
    Ast0.Default(def,colon,code) ->
      let arity =
	all_same false true tgt (mcode2line def)
	  [mcode2arity def; mcode2arity colon] in
      let def = mcode def in
      let colon = mcode colon in
      let code = dots (statement false arity) code in
      make_case_line c tgt arity (Ast0.Default(def,colon,code))
  | Ast0.Case(case,exp,colon,code) ->
      let arity =
	all_same false true tgt (mcode2line case)
	  [mcode2arity case; mcode2arity colon] in
      let case = mcode case in
      let exp = expression false arity exp in
      let colon = mcode colon in
      let code = dots (statement false arity) code in
      make_case_line c tgt arity (Ast0.Case(case,exp,colon,code))
  | Ast0.OptCase(_) -> failwith "unexpected OptCase"

(* --------------------------------------------------------------------- *)
(* CPP code *)

and make_define_body =
  let no_arity x = failwith "specific arity not allowed" in
  make_opt_unique no_arity no_arity no_arity

and define_body tgt s =
  match Ast0.unwrap s with
    Ast0.DMetaId(name,pure) ->
      let arity =
	all_same false false tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_define_body s tgt arity (Ast0.DMetaId(name,pure))
  | Ast0.DStm(stmtdots) ->
      let new_stmtdots = dots (statement false tgt) stmtdots in
      make_define_body s tgt tgt (Ast0.DStm(new_stmtdots))

(* --------------------------------------------------------------------- *)
(* Function declaration *)
(* Haven't thought much about arity here... *)

let top_level tgt t =
  Ast0.rewrap t
    (match Ast0.unwrap t with
      Ast0.FILEINFO(old_file,new_file) -> 
	if mcode2arity old_file = Ast0.NONE && mcode2arity new_file = Ast0.NONE
	then Ast0.FILEINFO(mcode old_file,mcode new_file)
	else fail t "unexpected arity for file info"
    | Ast0.DECL(stmt) ->
	Ast0.DECL(statement false tgt stmt)
    | Ast0.CODE(rule_elem_dots) ->
	Ast0.CODE(concat_dots (statement false tgt) rule_elem_dots)
    | Ast0.ERRORWORDS(exps) ->
	Ast0.ERRORWORDS(List.map (top_expression false false Ast0.NONE) exps)
    | Ast0.OTHER(_) -> fail t "eliminated by top_level")

let rule tgt = List.map (top_level tgt)

(* --------------------------------------------------------------------- *)
(* Entry points *)

let minus_arity code =
  rule Ast0.NONE code
