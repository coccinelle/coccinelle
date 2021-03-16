(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Arities matter for the minus slice, but not for the plus slice. *)

(* ? only allowed on rule_elems, and on subterms if the context is ? also. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)

let warning s = Printf.printf "warning: %s\n" s

let fail w str =
  failwith
    (Printf.sprintf "cocci line %d: %s"
       ((Ast0.get_info w).Ast0.pos_info.Ast0.line_start)
       str)

let make_opt optfn info tgt arity term =
  let term = Ast0.rewrap info term in
  if tgt = arity
  then term
  else (* tgt must be NONE *)
    match arity with
      Ast0.OPT -> Ast0.copywrap info (optfn term)
    | Ast0.NONE -> failwith "tgt must be NONE"

let all_same opt_allowed tgt line arities =
  let tgt =
    match tgt with
      Ast0.NONE ->
	(match List.hd arities with
	  Ast0.OPT when not opt_allowed ->
	    failwith
	      (Printf.sprintf
		 "%d: opt only allowed for the elements of a statement list"
		 line)
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

let mcode2line (_,_,info,_,_,_) = info.Ast0.pos_info.Ast0.line_start
let mcode2arity (_,arity,_,_,_,_) = arity
let mcodeassignOp2line op = match Ast0.unwrap op with
    Ast0.SimpleAssign op -> mcode2line op
  | Ast0.OpAssign op -> mcode2line op
  | Ast0.MetaAssign(mv,_,_) -> mcode2line mv

let mcodeassignOp2arity op = match Ast0.unwrap op with
    Ast0.SimpleAssign op -> mcode2arity op
  | Ast0.OpAssign op -> mcode2arity op
  | Ast0.MetaAssign(mv,_,_) -> mcode2arity mv

let mcodebinaryOp2line op = match Ast0.unwrap op with
    Ast0.Arith op -> mcode2line op
  | Ast0.Logical op -> mcode2line op
  | Ast0.MetaBinary(mv,_,_) -> mcode2line mv

let mcodebinaryOp2arity op = match Ast0.unwrap op with
    Ast0.Arith op -> mcode2arity op
  | Ast0.Logical op -> mcode2arity op
  | Ast0.MetaBinary(mv,_,_) -> mcode2arity mv

let mcode x = x (* nothing to do ... *)

(* --------------------------------------------------------------------- *)
(* Dots *)

let dots fn d = Ast0.rewrap d (List.map fn (Ast0.unwrap d))

(* --------------------------------------------------------------------- *)
(* Identifier *)

let make_id =
  make_opt
    (function x -> Ast0.OptIdent x)

let rec ident opt_allowed tgt i =
  match Ast0.unwrap i with
    Ast0.Id(name) ->
      let arity =
	all_same opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.Id(name))
  | Ast0.MetaId(name,constraints,seed,pure) ->
      let arity =
	all_same opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.MetaId(name,constraints,seed,pure))
  | Ast0.MetaFunc(name,constraints,pure) ->
      let arity =
	all_same opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.MetaFunc(name,constraints,pure))
  | Ast0.MetaLocalFunc(name,constraints,pure) ->
      let arity =
	all_same opt_allowed tgt (mcode2line name)
	  [mcode2arity name] in
      let name = mcode name in
      make_id i tgt arity (Ast0.MetaLocalFunc(name,constraints,pure))
  | Ast0.DisjId(starter,id_list,mids,ender) ->
      let id_list = List.map (ident opt_allowed tgt) id_list in
      (match List.rev id_list with
	_::xs ->
	  if anyopt xs (function Ast0.OptIdent(_) -> true | _ -> false)
	  then fail i "opt only allowed in the last disjunct"
      |	_ -> ());
      Ast0.rewrap i (Ast0.DisjId(starter,id_list,mids,ender))
  | Ast0.ConjId(starter,ids,mids,ender) ->
      (match ids with
	id::id_list ->
	  let id = ident opt_allowed tgt id in
	  let arity =
	    match Ast0.unwrap id with
	      Ast0.OptIdent _ -> Ast0.OPT
	    | _ -> Ast0.NONE in
	  let id_list = List.map (ident opt_allowed arity) id_list in
	  Ast0.rewrap i (Ast0.ConjId(starter,id::id_list,mids,ender))
      | _ -> i)
  | Ast0.OptIdent(_) | Ast0.AsIdent _ ->
      failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Expression *)

let make_exp =
  make_opt
    (function x -> Ast0.OptExp x)


let rec top_expression opt_allowed tgt expr =
  let exp_same = all_same opt_allowed tgt in
  match Ast0.unwrap expr with
    Ast0.Ident(id) ->
      let new_id = ident opt_allowed tgt id in
      Ast0.rewrap expr
	(match Ast0.unwrap new_id with
	  Ast0.OptIdent(id) ->
	    Ast0.OptExp(Ast0.rewrap expr (Ast0.Ident(id)))
	| _ -> Ast0.Ident(new_id))
  | Ast0.Constant(const) ->
      let arity = exp_same (mcode2line const) [mcode2arity const] in
      let const = mcode const in
      make_exp expr tgt arity (Ast0.Constant(const))
  | Ast0.StringConstant(lq,str,rq,isWchar) ->
      (* all components on the same line, so this is probably pointless... *)
      let arity = exp_same (mcode2line lq) [mcode2arity lq;mcode2arity rq] in
      let lq = mcode lq in
      let str = dots (string_fragment arity) str in
      let rq = mcode rq in
      make_exp expr tgt arity (Ast0.StringConstant(lq,str,rq,isWchar))
  | Ast0.FunCall(fn,lp,args,rp) ->
      let arity = exp_same (mcode2line lp) [mcode2arity lp;mcode2arity rp] in
      let fn = expression arity fn in
      let lp = mcode lp in
      let args = dots (expression arity) args in
      let rp = mcode rp in
      make_exp expr tgt arity (Ast0.FunCall(fn,lp,args,rp))
  | Ast0.Assignment(left,op,right,simple) ->
      let arity = exp_same (mcodeassignOp2line op) [mcodeassignOp2arity op] in
      let left = expression arity left in
      let op = mcode op in
      let right = expression arity right in
      make_exp expr tgt arity (Ast0.Assignment(left,op,right,simple))
  | Ast0.Sequence(left,op,right) ->
      let arity = exp_same (mcode2line op) [mcode2arity op] in
      let left = expression arity left in
      let op = mcode op in
      let right = expression arity right in
      make_exp expr tgt arity (Ast0.Sequence(left,op,right))
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
      let arity =
	exp_same (mcode2line why) [mcode2arity why; mcode2arity colon] in
      let exp1 = expression arity exp1 in
      let why = mcode why in
      let exp2 = get_option (expression arity) exp2 in
      let colon = mcode colon in
      let exp3 = expression arity exp3 in
      make_exp expr tgt arity (Ast0.CondExpr(exp1,why,exp2,colon,exp3))
  | Ast0.Postfix(exp,op) ->
      let arity = exp_same (mcode2line op) [mcode2arity op] in
      let exp = expression arity exp in
      let op = mcode op in
      make_exp expr tgt arity (Ast0.Postfix(exp,op))
  | Ast0.Infix(exp,op) ->
      let arity = exp_same (mcode2line op) [mcode2arity op] in
      let exp = expression arity exp in
      let op = mcode op in
      make_exp expr tgt arity (Ast0.Infix(exp,op))
  | Ast0.Unary(exp,op) ->
      let arity = exp_same (mcode2line op) [mcode2arity op] in
      let exp = expression arity exp in
      let op = mcode op in
      make_exp expr tgt arity (Ast0.Unary(exp,op))
  | Ast0.Binary(left,op,right) ->
      let arity = exp_same (mcodebinaryOp2line op) [mcodebinaryOp2arity op] in
      let left = expression arity left in
      let op = mcode op in
      let right = expression arity right in
      make_exp expr tgt arity (Ast0.Binary(left,op,right))
  | Ast0.Nested(left,op,right) -> failwith "nested in arity not possible"
  | Ast0.Paren(lp,exp,rp) ->
      let arity = exp_same (mcode2line lp) [mcode2arity lp;mcode2arity rp] in
      let lp = mcode lp in
      let exp = expression arity exp in
      let rp = mcode rp in
      make_exp expr tgt arity (Ast0.Paren(lp,exp,rp))
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
      let arity = exp_same (mcode2line lb) [mcode2arity lb; mcode2arity rb] in
      let exp1 = expression arity exp1 in
      let lb = mcode lb in
      let exp2 = expression arity exp2 in
      let rb = mcode rb in
      make_exp expr tgt arity (Ast0.ArrayAccess(exp1,lb,exp2,rb))
  | Ast0.RecordAccess(exp,pt,field) ->
      let arity = exp_same (mcode2line pt) [mcode2arity pt] in
      let exp = expression arity exp in
      let pt = mcode pt in
      let field = ident false arity field in
      make_exp expr tgt arity (Ast0.RecordAccess(exp,pt,field))
  | Ast0.RecordPtAccess(exp,ar,field) ->
      let arity = exp_same (mcode2line ar) [mcode2arity ar] in
      let exp = expression arity exp in
      let ar = mcode ar in
      let field = ident false arity field in
      make_exp expr tgt arity (Ast0.RecordPtAccess(exp,ar,field))
  | Ast0.Cast(lp,ty,attr,rp,exp) ->
      let arity =
	exp_same (mcode2line lp) (List.map mcode2arity ([lp;rp])) in
      let lp = mcode lp in
      let ty = typeC arity ty in
      let attr = List.map (attribute arity) attr in
      let rp = mcode rp in
      let exp = expression arity exp in
      make_exp expr tgt arity (Ast0.Cast(lp,ty,attr,rp,exp))
  | Ast0.SizeOfExpr(szf,exp) ->
      let arity = exp_same (mcode2line szf) [mcode2arity szf] in
      let szf = mcode szf in
      let exp = expression arity exp in
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
  | Ast0.MetaErr(name,constraints,pure)  ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_exp expr tgt arity (Ast0.MetaErr(name,constraints,pure))
  | Ast0.MetaExpr(name,constraints,ty,form,pure,bitfield)  ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_exp expr tgt arity
	(Ast0.MetaExpr(name,constraints,ty,form,pure,bitfield))
  | Ast0.MetaExprList(name,lenname,cstr,pure) ->
      let arity = exp_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_exp expr tgt arity (Ast0.MetaExprList(name,lenname,cstr,pure))
  | Ast0.EComma(cm)         ->
      let arity = exp_same (mcode2line cm) [mcode2arity cm] in
      let cm = mcode cm in
      make_exp expr tgt arity (Ast0.EComma(cm))
  | Ast0.DisjExpr(starter,exps,mids,ender) ->
      let exps = List.map (top_expression opt_allowed tgt) exps in
      (match List.rev exps with
	_::xs ->
	  if anyopt xs (function Ast0.OptExp(_) -> true | _ -> false)
	  then fail expr "opt only allowed in the last disjunct"
      |	_ -> ());
      Ast0.rewrap expr (Ast0.DisjExpr(starter,exps,mids,ender))
  | Ast0.ConjExpr(starter,exps,mids,ender) ->
      (match exps with
	e::es ->
	  let e = top_expression opt_allowed tgt e in
	  let arity =
	    match Ast0.unwrap e with
	      Ast0.OptExp _ -> Ast0.OPT
	    | _ -> Ast0.NONE in
	  let es = List.map (expression arity) es in
	  make_exp expr tgt arity (Ast0.ConjExpr(starter,e::es,mids,ender))
      |	_ -> expr)
  | Ast0.NestExpr(starter,exp_dots,ender,whencode,multi) ->
      let res =
        Ast0.NestExpr(starter,
                      dots (top_expression true Ast0.NONE) exp_dots,
                      ender,whencode,multi) in
      Ast0.rewrap expr res
  | Ast0.Edots(dots,whencode) ->
      let arity = exp_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode =
        get_option (fun (a,e,b) -> (a,e,expression Ast0.NONE b)) whencode in
      make_exp expr tgt arity (Ast0.Edots(dots,whencode))
  | Ast0.Constructor(lp,ty,rp,init) ->
      let arity = exp_same (mcode2line lp) [mcode2arity lp;mcode2arity rp] in
      let lp = mcode lp in
      let ty = typeC arity ty in
      let rp = mcode rp in
      let init = initialiser arity init in
      make_exp expr tgt arity (Ast0.Constructor(lp,ty,rp,init))
  (* why does optexp exist???? *)
  | Ast0.OptExp(_) | Ast0.AsExpr _ | Ast0.AsSExpr _ ->
      failwith "unexpected code"

and expression tgt exp = top_expression false tgt exp

and make_fragment =
  make_opt
    (function x -> failwith "opt not allowed for string fragment")

and string_fragment tgt e =
  match Ast0.unwrap e with
    Ast0.ConstantFragment(str) ->
      let arity = all_same false tgt (mcode2line str) [mcode2arity str] in
      let str = mcode str in
      make_fragment e tgt arity (Ast0.ConstantFragment(str))
  | Ast0.FormatFragment(pct,fmt) ->
      let arity = all_same false tgt (mcode2line pct) [mcode2arity pct] in
      let pct = mcode pct in
      let fmt = string_format arity fmt in
      make_fragment e tgt arity (Ast0.FormatFragment(pct,fmt))
  | Ast0.Strdots(dots) ->
      let arity = all_same false tgt (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      make_fragment e tgt arity (Ast0.Strdots(dots))
  | Ast0.MetaFormatList(pct,name,cstr,lenname) ->
      let arity =
	all_same false tgt (mcode2line pct)
	  [mcode2arity pct; mcode2arity name] in
      let pct = mcode pct in
      let name = mcode name in
      make_fragment e tgt arity (Ast0.MetaFormatList(pct,name,cstr,lenname))

and make_format =
  make_opt
    (function x -> failwith "opt not allowed for string format")

and string_format tgt e =
  match Ast0.unwrap e with
    Ast0.ConstantFormat(str) ->
      let arity = all_same false tgt (mcode2line str) [mcode2arity str] in
      let str = mcode str in
      make_format e tgt arity (Ast0.ConstantFormat(str))
  | Ast0.MetaFormat(name,constraints) ->
      let arity = all_same false tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_format e tgt arity (Ast0.MetaFormat(name,constraints))

(* --------------------------------------------------------------------- *)
(* Types *)

and make_typeC =
  make_opt
    (function x -> Ast0.OptType x)

and top_typeC tgt opt_allowed typ =
  match Ast0.unwrap typ with
    Ast0.ConstVol(cv,ty) ->
      let arity = all_same opt_allowed tgt (mcode2line cv)
	  [mcode2arity cv] in
      let cv = mcode cv in
      let ty = typeC arity ty in
      make_typeC typ tgt arity (Ast0.ConstVol(cv,ty))
  | Ast0.BaseType(ty,strings) ->
      let arity =
	all_same opt_allowed tgt (mcode2line (List.hd strings))
	  (List.map mcode2arity strings) in
      let strings = List.map mcode strings in
      make_typeC typ tgt arity (Ast0.BaseType(ty,strings))
  | Ast0.Signed(sign,ty) ->
      let arity =
	all_same opt_allowed tgt (mcode2line sign) [mcode2arity sign] in
      let sign = mcode sign in
      let ty = get_option (typeC arity) ty in
      make_typeC typ tgt arity (Ast0.Signed(sign,ty))
  | Ast0.Pointer(ty,star) ->
      let arity =
	all_same opt_allowed tgt (mcode2line star) [mcode2arity star] in
      let ty = typeC arity ty in
      let star = mcode star in
      make_typeC typ tgt arity (Ast0.Pointer(ty,star))
  | Ast0.ParenType(lp,ty,rp) ->
      let arity =
        all_same opt_allowed tgt (mcode2line lp)
        [mcode2arity lp; mcode2arity rp] in
      let lp = mcode lp in
      let ty = typeC arity ty in
      let rp = mcode rp in
      make_typeC typ tgt arity (Ast0.ParenType(lp,ty,rp))
  | Ast0.FunctionType(ty,lp,params,rp) ->
      let arity =
        all_same opt_allowed tgt (mcode2line lp)
        [mcode2arity lp; mcode2arity rp] in
      let ty = typeC arity ty in
      let lp = mcode lp in
      let params = parameter_list tgt params in
      let rp = mcode rp in
      make_typeC typ tgt arity (Ast0.FunctionType(ty,lp,params,rp))
  | Ast0.Array(ty,lb,size,rb) ->
      let arity =
	all_same opt_allowed tgt (mcode2line lb)
	  [mcode2arity lb;mcode2arity rb] in
      let ty = typeC arity ty in
      let lb = mcode lb in
      let size = get_option (expression arity) size in
      let rb = mcode rb in
      make_typeC typ tgt arity (Ast0.Array(ty,lb,size,rb))
  | Ast0.Decimal(dec,lp,length,comma,precision_opt,rp) ->
      let arity =
	all_same opt_allowed tgt (mcode2line dec)
	  [mcode2arity dec;mcode2arity lp;mcode2arity rp] in
      let dec = mcode dec in
      let lp = mcode lp in
      let length = expression arity length in
      let comma = get_option mcode comma in
      let precision_opt = get_option (expression arity) precision_opt in
      let rp = mcode rp in
      make_typeC typ tgt arity
	(Ast0.Decimal(dec,lp,length,comma,precision_opt,rp))
  | Ast0.EnumName(kind,name) ->
      let arity =
	all_same opt_allowed tgt (mcode2line kind) [mcode2arity kind] in
      let kind = mcode kind in
      let name = get_option (ident false arity) name in
      make_typeC typ tgt arity (Ast0.EnumName(kind,name))
  | Ast0.EnumDef(ty,lb,decls,rb) ->
      let arity =
	all_same opt_allowed tgt (mcode2line lb)
	  (List.map mcode2arity [lb;rb]) in
      let ty = typeC arity ty in
      let lb = mcode lb in
      let ids = dots (enum_decl tgt) decls in
      let rb = mcode rb in
      make_typeC typ tgt arity (Ast0.EnumDef(ty,lb,ids,rb))
  | Ast0.StructUnionName(kind,name) ->
      let arity =
	all_same opt_allowed tgt (mcode2line kind)
	  [mcode2arity kind] in
      let kind = mcode kind in
      let name = get_option (ident false arity) name in
      make_typeC typ tgt arity (Ast0.StructUnionName(kind,name))
  | Ast0.StructUnionDef(ty,lb,decls,rb) ->
      let arity =
	all_same opt_allowed tgt (mcode2line lb)
	  (List.map mcode2arity [lb;rb]) in
      let ty = typeC arity ty in
      let lb = mcode lb in
      let decls = dots (field tgt) decls in
      let rb = mcode rb in
      make_typeC typ tgt arity (Ast0.StructUnionDef(ty,lb,decls,rb))
  | Ast0.TypeOfExpr(tf,lp,exp,rp) ->
      let arity =
	all_same opt_allowed tgt (mcode2line tf)
	  (List.map mcode2arity [tf;lp;rp]) in
      let tf = mcode tf in
      let lp = mcode lp in
      let exp = expression arity exp in
      let rp = mcode rp in
      make_typeC typ tgt arity (Ast0.TypeOfExpr(tf,lp,exp,rp))
  | Ast0.TypeOfType(tf,lp,ty,rp) ->
      let arity =
	all_same opt_allowed tgt (mcode2line tf)
	  (List.map mcode2arity [tf;lp;rp]) in
      let tf = mcode tf in
      let lp = mcode lp in
      let ty = typeC arity ty in
      let rp = mcode rp in
      make_typeC typ tgt arity (Ast0.TypeOfType(tf,lp,ty,rp))
  | Ast0.TypeName(name) ->
      let arity =
	all_same opt_allowed tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_typeC typ tgt arity (Ast0.TypeName(name))
  | Ast0.AutoType(auto) ->
      let arity =
	all_same opt_allowed tgt (mcode2line auto) [mcode2arity auto] in
      let auto = mcode auto in
      make_typeC typ tgt arity (Ast0.AutoType(auto))
  | Ast0.MetaType(name,cstr,pure) ->
      let arity =
	all_same opt_allowed tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_typeC typ tgt arity (Ast0.MetaType(name,cstr,pure))
  | Ast0.DisjType(starter,types,mids,ender) ->
      let types = List.map (typeC tgt) types in
      (match List.rev types with
	_::xs ->
	  if anyopt xs (function Ast0.OptType(_) -> true | _ -> false)
	  then fail typ "opt only allowed in the last disjunct"
      |	_ -> ());
      let res = Ast0.DisjType(starter,types,mids,ender) in
      Ast0.rewrap typ res
  | Ast0.ConjType(starter,types,mids,ender) ->
      (match types with
	t::ts ->
	  let t = typeC tgt t in
	  let arity =
	    match Ast0.unwrap t with
	      Ast0.OptType _ -> Ast0.OPT
	    | _ -> Ast0.NONE in
	  let ts = List.map (typeC arity) ts in
	  let res = Ast0.ConjType(starter,t::ts,mids,ender) in
	  Ast0.rewrap typ res
      |	_ -> typ)
  | Ast0.OptType(_) | Ast0.AsType _ ->
      failwith "unexpected code"

and typeC tgt ty = top_typeC tgt false ty

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and make_decl =
  make_opt
    (function x -> Ast0.OptDecl x)

and declaration tgt decl =
  match Ast0.unwrap decl with
    Ast0.MetaDecl(name,cstr,pure) ->
      let arity = all_same true tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_decl decl tgt arity (Ast0.MetaDecl(name,cstr,pure))
  | Ast0.Init(stg,ty,id,attr,eq,exp,sem) ->
      let arity =
	all_same true tgt (mcode2line eq)
	  ((match stg with None -> [] | Some x -> [mcode2arity x]) @
	   (List.map mcode2arity [eq;sem])) in
      let stg = get_option mcode stg in
      let ty = typeC arity ty in
      let id = ident false arity id in
      let attr = List.map (attribute arity) attr in
      let eq = mcode eq in
      let exp = initialiser arity exp in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.Init(stg,ty,id,attr,eq,exp,sem))
  | Ast0.UnInit(stg,ty,id,attr,sem) ->
      let arity =
	all_same true tgt (mcode2line sem)
	  ((match stg with None -> [] | Some x -> [mcode2arity x]) @
	   [mcode2arity sem]) in
      let stg = get_option mcode stg in
      let ty = typeC arity ty in
      let id = ident false arity id in
      let attr = List.map (attribute arity) attr in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.UnInit(stg,ty,id,attr,sem))
  | Ast0.FunProto(fi,name,lp1,params,va,rp1,sem) ->
    let tokens = match va with
      | None -> [lp1;rp1;sem]
      | Some (c1,e1) -> [lp1;c1;e1;rp1;sem] in
      let arity =
	all_same true tgt (mcode2line lp1)
	  (List.map mcode2arity tokens) in
      let fi = List.map (fninfo arity) fi in
      let name = ident false arity name in
      let lp1 = mcode lp1 in
      let params = parameter_list tgt params in
      let va = match va with
        | None -> None
        | Some (c1, e1) -> Some (mcode c1, mcode e1) in
      let rp1 = mcode rp1 in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.FunProto(fi,name,lp1,params,va,rp1,sem))
  | Ast0.MacroDecl(stg,name,lp,args,rp,attr,sem) ->
      let arity =
	all_same true tgt (mcode2line lp)
	  ((match stg with None -> [] | Some x -> [mcode2arity x]) @
	   (List.map mcode2arity ([lp;rp;sem]))) in
      let stg = get_option mcode stg in
      let name = ident false arity name in
      let lp = mcode lp in
      let args = dots (expression arity) args in
      let rp = mcode rp in
      let attr = List.map (attribute arity) attr in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.MacroDecl(stg,name,lp,args,rp,attr,sem))
  | Ast0.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) ->
      let arity =
	all_same true tgt (mcode2line lp)
	  ((match stg with None -> [] | Some x -> [mcode2arity x]) @
	   (List.map mcode2arity [lp;rp;eq;sem])) in
      let stg = get_option mcode stg in
      let name = ident false arity name in
      let lp = mcode lp in
      let args = dots (expression arity) args in
      let rp = mcode rp in
      let ini = initialiser arity ini in
      let sem = mcode sem in
      make_decl decl tgt arity
	(Ast0.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem))
  | Ast0.TyDecl(ty,attr,sem) ->
      let arity =
	all_same true tgt
	  (mcode2line sem) [mcode2arity sem] in
      let ty = typeC arity ty in
      let attr = List.map (attribute arity) attr in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.TyDecl(ty,attr,sem))
  | Ast0.Typedef(stg,ty,id,sem) ->
      let arity =
	all_same true tgt (mcode2line sem)
	  [mcode2arity stg;mcode2arity sem] in
      let stg = mcode stg in
      let ty = typeC arity ty in
      let id = typeC arity id in
      let sem = mcode sem in
      make_decl decl tgt arity (Ast0.Typedef(stg,ty,id,sem))
  | Ast0.DisjDecl(starter,decls,mids,ender) ->
      let decls = List.map (declaration tgt) decls in
      (match List.rev decls with
	_::xs ->
	  if anyopt xs (function Ast0.OptDecl(_) -> true | _ -> false)
	  then fail decl "opt only allowed in the last disjunct"
      |	_ -> ());
      let res = Ast0.DisjDecl(starter,decls,mids,ender) in
      Ast0.rewrap decl res
  | Ast0.ConjDecl(starter,decls,mids,ender) ->
      let decls = List.map (declaration tgt) decls in
      (if anyopt decls (function Ast0.OptDecl(_) -> true | _ -> false)
      then failwith "unexpected code");
      let res = Ast0.ConjDecl(starter,decls,mids,ender) in
      Ast0.rewrap decl res
  | Ast0.OptDecl(_) | Ast0.AsDecl _ ->
      failwith "unexpected code"

(* --------------------------------------------------------------------- *)
(* Field declaration *)

and make_field =
  make_opt
    (function x -> Ast0.OptField x)

and field tgt decl =
  match Ast0.unwrap decl with
    Ast0.MetaField(name,cstr,pure) ->
      let arity = all_same true tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_field decl tgt arity (Ast0.MetaField(name,cstr,pure))
  | Ast0.MetaFieldList(name,lenname,cstr,pure) ->
      let arity = all_same true tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_field decl tgt arity (Ast0.MetaFieldList(name,lenname,cstr,pure))
  | Ast0.Field(ty,id,bf,sem) ->
      let arity = all_same true tgt (mcode2line sem) [mcode2arity sem] in
      let ty = typeC arity ty in
      let id = Common.map_option (ident false arity) id in
      let bitfield (c, e) = (mcode c, expression arity e) in
      let bf = Common.map_option bitfield bf in
      let sem = mcode sem in
      make_field decl tgt arity (Ast0.Field(ty,id,bf,sem))
  | Ast0.DisjField(starter,decls,mids,ender) ->
      let decls = List.map (field tgt) decls in
      (match List.rev decls with
	_::xs ->
	  if anyopt xs (function Ast0.OptField(_) -> true | _ -> false)
	  then fail decl "opt only allowed in the last disjunct"
      |	_ -> ());
      let res = Ast0.DisjField(starter,decls,mids,ender) in
      Ast0.rewrap decl res
  | Ast0.ConjField(starter,decls,mids,ender) ->
      let decls = List.map (field tgt) decls in
      (if anyopt decls (function Ast0.OptField(_) -> true | _ -> false)
      then failwith "unexpected code");
      let res = Ast0.ConjField(starter,decls,mids,ender) in
      Ast0.rewrap decl res
  | Ast0.Fdots(dots,whencode) ->
      let arity = all_same true tgt (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode =
        get_option (fun (a,e,b) -> (a,e,field Ast0.NONE b)) whencode in
      make_field decl tgt arity (Ast0.Fdots(dots,whencode))
  | Ast0.OptField(_) ->
      failwith "unexpected code"

and enum_decl tgt decl =
  match Ast0.unwrap decl with
    Ast0.Enum(name,enum_val) ->
      let name = ident true tgt name in
      let enum_val =
        get_option
          (fun (eq,eval) ->
             let arity = all_same true tgt (mcode2line eq) [mcode2arity eq] in
             (mcode eq, expression arity eval)) enum_val in
      let res = Ast0.Enum(name,enum_val) in
      Ast0.rewrap decl res
  | Ast0.EnumComma(cm) ->
      (*let arity = all_same true tgt (mcode2line cm) [mcode2arity cm] in*)
      let cm = mcode cm in
      let res = Ast0.EnumComma(cm) in
      Ast0.rewrap decl res
  | Ast0.EnumDots(dots,whencode) ->
      let dots = mcode dots in
      let whencode =
        get_option (fun (a,e,b) -> (a,e,enum_decl Ast0.NONE b)) whencode in
      let res = Ast0.EnumDots(dots,whencode) in
      Ast0.rewrap decl res

(* --------------------------------------------------------------------- *)
(* Initializer *)

and make_init =
  make_opt
    (function x -> Ast0.OptIni x)

and initialiser tgt i =
  let init_same = all_same true tgt in
  match Ast0.unwrap i with
    Ast0.MetaInit(name,cstr,pure) ->
      let arity = init_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_init i tgt arity (Ast0.MetaInit(name,cstr,pure))
  | Ast0.MetaInitList(name,lenname,cstr,pure) ->
      let arity = init_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_init i tgt arity (Ast0.MetaInitList(name,lenname,cstr,pure))
  | Ast0.InitExpr(exp) ->
      Ast0.rewrap i (Ast0.InitExpr(expression tgt exp))
  | Ast0.InitList(lb,initlist,rb,ordered) ->
      let arity = init_same (mcode2line lb) [mcode2arity lb; mcode2arity rb] in
      let lb = mcode lb in
      let initlist = dots (initialiser arity) initlist in
      let rb = mcode rb in
      make_init i tgt arity (Ast0.InitList(lb,initlist,rb,ordered))
  | Ast0.InitGccExt(designators,eq,ini) ->
      let arity = init_same (mcode2line eq) [mcode2arity eq] in
      let designators = List.map (designator arity) designators in
      let eq = mcode eq in
      let ini = initialiser arity ini in
      make_init i tgt arity (Ast0.InitGccExt(designators,eq,ini))
  | Ast0.InitGccName(name,eq,ini) ->
      let arity = init_same (mcode2line eq) [mcode2arity eq] in
      let name = ident true arity name in
      let eq = mcode eq in
      let ini = initialiser arity ini in
      make_init i tgt arity (Ast0.InitGccName(name,eq,ini))
  | Ast0.IComma(cm) ->
      let arity = init_same (mcode2line cm) [mcode2arity cm] in
      let cm = mcode cm in
      make_init i tgt arity (Ast0.IComma(cm))
  | Ast0.Idots(dots,whencode) ->
      let arity = init_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      let whencode =
        get_option (fun (a,e,b) -> (a,e,initialiser Ast0.NONE b)) whencode in
      make_init i tgt arity (Ast0.Idots(dots,whencode))
  | Ast0.OptIni(_) | Ast0.AsInit _ ->
      failwith "unexpected code"

and designator tgt d =
  let dsame = all_same false tgt in
  match d with
    Ast0.DesignatorField(dot,id) ->
      let arity = dsame (mcode2line dot) [mcode2arity dot] in
      let dot = mcode dot in
      let id = ident false arity id in
      Ast0.DesignatorField(dot,id)
  | Ast0.DesignatorIndex(lb,exp,rb) ->
      let arity = dsame (mcode2line lb) [mcode2arity lb;mcode2arity rb] in
      let lb = mcode lb in
      let exp = top_expression false arity exp in
      let rb = mcode rb in
      Ast0.DesignatorIndex(lb,exp,rb)
  | Ast0.DesignatorRange(lb,min,dots,max,rb) ->
      let arity =
	dsame (mcode2line lb)
	  [mcode2arity lb;mcode2arity dots;mcode2arity rb] in
      let lb = mcode lb in
      let min = top_expression false arity min in
      let dots = mcode dots in
      let max = top_expression false arity max in
      let rb = mcode rb in
      Ast0.DesignatorRange(lb,min,dots,max,rb)

(* --------------------------------------------------------------------- *)
(* Parameter *)

and make_param =
  make_opt
    (function x -> Ast0.OptParam x)

and parameterTypeDef tgt param =
  let param_same = all_same true tgt in
  match Ast0.unwrap param with
    Ast0.VoidParam(ty,attr) ->
      Ast0.rewrap param
        (Ast0.VoidParam(typeC tgt ty,List.map (attribute tgt) attr))
  | Ast0.Param(ty,Some id,attr) ->
      let ty = top_typeC tgt true ty in
      let id = ident true tgt id in
      let attr = List.map (attribute tgt) attr in
      Ast0.rewrap param
	(match (Ast0.unwrap ty,Ast0.unwrap id) with
	  (Ast0.OptType(ty),Ast0.OptIdent(id)) ->
	    Ast0.OptParam(Ast0.rewrap param (Ast0.Param(ty,Some id,attr)))
	| (Ast0.OptType(ty),_) ->
	    fail param "arity mismatch in param declaration"
	| (_,Ast0.OptIdent(id)) ->
	    fail param "arity mismatch in param declaration"
	| _ -> Ast0.Param(ty,Some id,attr))
  | Ast0.Param(ty,None,attr) ->
      let ty = top_typeC tgt true ty in
      let attr = List.map (attribute tgt) attr in
      Ast0.rewrap param
	(match Ast0.unwrap ty with
	  Ast0.OptType(ty) ->
	    Ast0.OptParam(Ast0.rewrap param (Ast0.Param(ty,None,attr)))
	| _ -> Ast0.Param(ty,None,attr))
  | Ast0.MetaParam(name,cstr,pure) ->
      let arity = param_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_param param tgt arity (Ast0.MetaParam(name,cstr,pure))
  | Ast0.MetaParamList(name,lenname,cstr,pure) ->
      let arity = param_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_param param tgt arity (Ast0.MetaParamList(name,lenname,cstr,pure))
  | Ast0.PComma(cm) ->
      let arity = param_same (mcode2line cm) [mcode2arity cm] in
      let cm = mcode cm in
      make_param param tgt arity (Ast0.PComma(cm))
  | Ast0.Pdots(dots) ->
      let arity = param_same (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      make_param param tgt arity (Ast0.Pdots(dots))
  | Ast0.OptParam(_) | Ast0.AsParam _ ->
      failwith "unexpected code"

and parameter_list tgt = dots (parameterTypeDef tgt)

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and make_rule_elem x =
  make_opt
    (function x -> Ast0.OptStm x)
    x

and statement tgt stm =
  let stm_same = all_same true tgt in
  match Ast0.unwrap stm with
    Ast0.Decl(bef,decl) ->
      let new_decl = declaration tgt decl in
      Ast0.rewrap stm
	(match Ast0.unwrap new_decl with
	  Ast0.OptDecl(decl) ->
	    Ast0.OptStm(Ast0.rewrap stm (Ast0.Decl(bef,decl)))
	| _ -> Ast0.Decl(bef,new_decl))
  | Ast0.Seq(lbrace,body,rbrace) ->
      let arity =
	stm_same (mcode2line lbrace)
	  [mcode2arity lbrace; mcode2arity rbrace] in
      let lbrace = mcode lbrace in
      let body = dots (statement arity) body in
      let rbrace = mcode rbrace in
      make_rule_elem stm tgt arity (Ast0.Seq(lbrace,body,rbrace))
  | Ast0.ExprStatement(exp,sem) ->
      let arity = stm_same (mcode2line sem) [mcode2arity sem] in
      let exp = get_option (expression arity) exp in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.ExprStatement(exp,sem))
  | Ast0.IfThen(iff,lp,exp,rp,branch,aft) ->
      let arity =
	stm_same (mcode2line iff) (List.map mcode2arity [iff;lp;rp]) in
      let iff = mcode iff in
      let lp = mcode lp in
      let exp = expression arity exp in
      let rp = mcode rp in
      let branch = statement arity branch in
      make_rule_elem stm tgt arity (Ast0.IfThen(iff,lp,exp,rp,branch,aft))
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) ->
      let arity =
	stm_same (mcode2line iff) (List.map mcode2arity [iff;lp;rp;els]) in
      let iff = mcode iff in
      let lp = mcode lp in
      let exp = expression arity exp in
      let rp = mcode rp in
      let branch1 = statement arity branch1 in
      let els = mcode els in
      let branch2 = statement arity branch2 in
      make_rule_elem stm tgt arity
	(Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft))
  | Ast0.While(wh,lp,exp,rp,body,aft) ->
      let arity =
	stm_same (mcode2line wh)
	  (List.map mcode2arity [wh;lp;rp]) in
      let wh = mcode wh in
      let lp = mcode lp in
      let exp = expression arity exp in
      let rp = mcode rp in
      let body = statement arity body in
      make_rule_elem stm tgt arity (Ast0.While(wh,lp,exp,rp,body,aft))
  | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
      let arity =
	stm_same (mcode2line wh) (List.map mcode2arity [d;wh;lp;rp;sem]) in
      let d = mcode d in
      let body = statement arity body in
      let wh = mcode wh in
      let lp = mcode lp in
      let exp = expression arity exp in
      let rp = mcode rp in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.Do(d,body,wh,lp,exp,rp,sem))
  | Ast0.For(fr,lp,first,exp2,sem2,exp3,rp,body,aft) ->
      let arity =
	let mcodes = [fr;lp;sem2;rp] in
	let mcodes =
	  match Ast0.unwrap first with
	    Ast0.ForExp(exp1,sem1) -> sem1::mcodes
	  | Ast0.ForDecl _ -> mcodes in
	stm_same (mcode2line fr) (List.map mcode2arity mcodes) in
      let fr = mcode fr in
      let lp = mcode lp in
      let first =
	match Ast0.unwrap first with
	  Ast0.ForExp(exp1,sem1) ->
	    let exp1 = get_option (expression arity) exp1 in
	    let sem1 = mcode sem1 in
	    Ast0.rewrap first (Ast0.ForExp(exp1,sem1))
	| Ast0.ForDecl (bef,decl) ->
	    Ast0.rewrap first
	      (Ast0.ForDecl(bef,declaration arity decl)) in
      let exp2 = get_option (expression arity) exp2 in
      let sem2 = mcode sem2 in
      let exp3 = get_option (expression arity) exp3 in
      let rp = mcode rp in
      let body = statement arity body in
      make_rule_elem stm tgt arity
	(Ast0.For(fr,lp,first,exp2,sem2,exp3,rp,body,aft))
  | Ast0.Iterator(nm,lp,args,rp,body,aft) ->
      let arity = stm_same (mcode2line lp) (List.map mcode2arity [lp;rp]) in
      let nm = ident false arity nm in
      let lp = mcode lp in
      let args = dots (expression arity) args in
      let rp = mcode rp in
      let body = statement arity body in
      make_rule_elem stm tgt arity (Ast0.Iterator(nm,lp,args,rp,body,aft))
  | Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) ->
      let arity =
	stm_same (mcode2line switch)
	  (List.map mcode2arity [switch;lp;rp;lb;rb]) in
      let switch = mcode switch in
      let lp = mcode lp in
      let exp = expression arity exp in
      let rp = mcode rp in
      let lb = mcode lb in
      let decls = dots (statement arity) decls in
      let cases = dots (case_line arity) cases in
      let rb = mcode rb in
      make_rule_elem stm tgt arity
	(Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb))
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
  | Ast0.Label(l,dd) ->
      let arity = mcode2arity dd in
      let l = ident false tgt l in
      let dd = mcode dd in
      make_rule_elem stm tgt arity (Ast0.Label(l,dd))
  | Ast0.Goto(goto,l,sem) ->
      let arity =
	stm_same (mcode2line goto) (List.map mcode2arity [goto;sem]) in
      let goto = mcode goto in
      let l = ident false arity l in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.Goto(goto,l,sem))
  | Ast0.Return(ret,sem) ->
      let arity = stm_same (mcode2line ret) (List.map mcode2arity [ret;sem]) in
      let ret = mcode ret in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.Return(ret,sem))
  | Ast0.ReturnExpr(ret,exp,sem) ->
      let arity = stm_same (mcode2line ret) (List.map mcode2arity [ret;sem]) in
      let ret = mcode ret in
      let exp = expression arity exp in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.ReturnExpr(ret,exp,sem))
  | Ast0.Exec(exec,lang,code,sem) ->
      let arity =
	stm_same (mcode2line exec) (List.map mcode2arity [exec;lang;sem]) in
      let exec = mcode exec in
      let lang = mcode lang in
      let code = dots (exec_code arity) code in
      let sem = mcode sem in
      make_rule_elem stm tgt arity (Ast0.Exec(exec,lang,code,sem))
  | Ast0.MetaStmt(name,cstr,pure) ->
      let arity = stm_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_rule_elem stm tgt arity (Ast0.MetaStmt(name,cstr,pure))
  | Ast0.MetaStmtList(name,lenname,cstr,pure) ->
      let arity = stm_same (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_rule_elem stm tgt arity (Ast0.MetaStmtList(name,lenname,cstr,pure))
  | Ast0.Exp(exp) ->
      let new_exp = top_expression true tgt exp in
      Ast0.rewrap stm
	(match Ast0.unwrap new_exp with
	  Ast0.OptExp(exp) ->
	    Ast0.OptStm(Ast0.rewrap stm (Ast0.Exp(exp)))
	| _ -> Ast0.Exp(new_exp))
  | Ast0.TopExp(exp) ->
      let new_exp = top_expression true tgt exp in
      Ast0.rewrap stm
	(match Ast0.unwrap new_exp with
	  Ast0.OptExp(exp) ->
	    Ast0.OptStm(Ast0.rewrap stm (Ast0.TopExp(exp)))
	| _ -> Ast0.TopExp(new_exp))
  | Ast0.Ty(ty) ->
      let new_ty = typeC tgt ty in (* opt makes no sense alone at top level *)
      Ast0.rewrap stm
	(match Ast0.unwrap new_ty with
	  Ast0.OptType(ty) ->
	    Ast0.OptStm(Ast0.rewrap stm (Ast0.Ty(ty)))
	| _ -> Ast0.Ty(new_ty))
  | Ast0.TopId(id) ->
      (* opt makes no sense alone at top level *)
      let new_id = ident false tgt id in
      Ast0.rewrap stm
	(match Ast0.unwrap new_id with
	  Ast0.OptIdent(id) ->
	    Ast0.OptStm(Ast0.rewrap stm (Ast0.TopId(id)))
	| _ -> Ast0.TopId(new_id))
  | Ast0.TopInit(init) ->
      let new_init = initialiser tgt init in
      Ast0.rewrap stm
	(match Ast0.unwrap new_init with
	  Ast0.OptIni(init) ->
	    Ast0.OptStm(Ast0.rewrap stm (Ast0.TopInit(init)))
	| _ -> Ast0.TopInit(new_init))
  | Ast0.Disj(starter,rule_elem_dots_list,mids,ender) ->
      let stms =
	List.map (function x -> dots (statement tgt) x)
	  rule_elem_dots_list in
      let (found_opt,unopt) =
	List.fold_left
	  (function (found_opt,lines) ->
	    function x ->
	      let rebuild l =
		(* previously just checked the last thing in the list,
		   but everything should be optional for the whole thing to
		   be optional *)
		let is_opt x =
		  match Ast0.unwrap x with
		    Ast0.OptStm(x) -> true
		  | _ -> false in
		let unopt x =
		  match Ast0.unwrap x with
		    Ast0.OptStm(x) -> x
		  | _ -> x in
		if List.for_all is_opt l
		then (true,List.map unopt l)
		else (false, l) in
	      let (l,k) = (Ast0.unwrap x, function l -> Ast0.rewrap x l) in
	      let (found_opt,l) = rebuild l in
	      (found_opt,(k l)::lines))
	  (false,[]) stms in
      let unopt = List.rev unopt in
      if found_opt
      then
	make_rule_elem stm tgt Ast0.OPT (Ast0.Disj(starter,unopt,mids,ender))
      else Ast0.rewrap stm (Ast0.Disj(starter,stms,mids,ender))
  | Ast0.Conj(starter,rule_elem_dots_list,mids,ender) ->
      let stms =
	List.map (function x -> dots (statement tgt) x)
	  rule_elem_dots_list in
      let (found_opt,unopt) =
	List.fold_left
	  (function (found_opt,lines) ->
	    function x ->
	      let rebuild l =
		(* previously just checked the last thing in the list,
		   but everything should be optional for the whole thing to
		   be optional *)
		let is_opt x =
		  match Ast0.unwrap x with
		    Ast0.OptStm(x) -> true
		  | _ -> false in
		let unopt x =
		  match Ast0.unwrap x with
		    Ast0.OptStm(x) -> x
		  | _ -> x in
		if List.for_all is_opt l
		then (true,List.map unopt l)
		else (false, l) in
	      let (l,k) = (Ast0.unwrap x, function l -> Ast0.rewrap x l) in
	      let (fo,l) = rebuild l in
	      (found_opt && fo,(k l)::lines))
	  (true,[]) stms in
      let unopt = List.rev unopt in
      if found_opt
      then
	make_rule_elem stm tgt Ast0.OPT (Ast0.Disj(starter,unopt,mids,ender))
      else Ast0.rewrap stm (Ast0.Conj(starter,stms,mids,ender))
  | Ast0.Nest(starter,rule_elem_dots,ender,whn,multi) ->
      let new_rule_elem_dots =
	dots (statement Ast0.NONE) rule_elem_dots in
      let whn =
	List.map
	  (whencode (dots (statement Ast0.NONE)) (statement Ast0.NONE)
	     (expression Ast0.NONE))
	  whn in
      Ast0.rewrap stm
	(Ast0.Nest(starter,new_rule_elem_dots,ender,whn,multi))
  | Ast0.Dots(d,whn)    ->
      let arity = stm_same (mcode2line d) [mcode2arity d] in
      let d = mcode d in
      let whn =
	List.map
	  (whencode (dots (statement Ast0.NONE)) (statement Ast0.NONE)
	     (expression Ast0.NONE))
	  whn in
      make_rule_elem stm tgt arity (Ast0.Dots(d,whn))
  | Ast0.FunDecl(bef,fi,name,lp,params,va,rp,lbrace,body,rbrace,aft) ->
      let arity =
	all_same true tgt (mcode2line lp)
	  ((List.map mcode2arity [lp;rp;lbrace;rbrace]) @ (fninfo2arity fi)) in
      let fi = List.map (fninfo arity) fi in
      let name = ident false arity name in
      let lp = mcode lp in
      let params = parameter_list arity params in
      let newva = match va with
        | None -> None
        | Some (comma, ellipsis) -> Some (mcode comma, mcode ellipsis) in
      let rp = mcode rp in
      let lbrace = mcode lbrace in
      let body = dots (statement arity) body in
      let rbrace = mcode rbrace in
      make_rule_elem stm tgt arity
	(Ast0.FunDecl(bef,fi,name,lp,params,newva,rp,lbrace,body,rbrace,aft))
  | Ast0.Include(inc,s) ->
      let arity =
	all_same true tgt (mcode2line inc) [mcode2arity inc; mcode2arity s] in
      let inc = mcode inc in
      let s = mcode s in
      make_rule_elem stm tgt arity (Ast0.Include(inc,s))
  | Ast0.MetaInclude(inc,s) ->
      let arity = all_same true tgt (mcode2line inc) [mcode2arity inc] in
      let inc = mcode inc in
      let s = expression arity s in
      make_rule_elem stm tgt arity (Ast0.MetaInclude(inc,s))
  | Ast0.Undef(def,id) ->
      let arity = all_same true tgt (mcode2line def) [mcode2arity def] in
      let def = mcode def in
      let id = ident false arity id in
      make_rule_elem stm tgt arity (Ast0.Undef(def,id))
  | Ast0.Define(def,id,params,body) ->
      let arity = all_same true tgt (mcode2line def) [mcode2arity def] in
      let def = mcode def in
      let id = ident false arity id in
      let params = define_parameters arity params in
      let body = dots (statement arity) body in
      make_rule_elem stm tgt arity (Ast0.Define(def,id,params,body))
  | Ast0.Pragma(prg,id,body) ->
      let arity = all_same true tgt (mcode2line prg) [mcode2arity prg] in
      let prg = mcode prg in
      let id = ident false arity id in
      let body = pragmainfo arity body in
      make_rule_elem stm tgt arity (Ast0.Pragma(prg,id,body))
  | Ast0.OptStm(_) | Ast0.AsStmt _ ->
      failwith "unexpected code"

and make_pragma =
  make_opt
    (function x -> failwith "opt not allowed for pragma")

and pragmainfo tgt pi =
  match Ast0.unwrap pi with
    Ast0.PragmaString(s) ->
      let arity = all_same false tgt (mcode2line s) [mcode2arity s] in
      let s = mcode s in
      make_pragma pi tgt arity (Ast0.PragmaString(s))
  | Ast0.PragmaDots (dots) ->
      let arity = all_same false tgt (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      make_pragma pi tgt arity (Ast0.PragmaDots (dots))

and define_parameters tgt params =
  match Ast0.unwrap params with
    Ast0.NoParams -> params
  | Ast0.DParams(lp,params,rp) ->
      let arity =
	all_same true tgt (mcode2line lp) [mcode2arity lp;mcode2arity rp] in
      let lp = mcode lp in
      let params = dots (define_param arity) params in
      let rp = mcode rp in
      Ast0.rewrap params (Ast0.DParams(lp,params,rp))

and make_define_param x =
  make_opt
    (function x -> Ast0.OptDParam x)
    x

and define_param tgt param =
  match Ast0.unwrap param with
    Ast0.DParam(id) ->
      let new_id = ident true tgt id in
      Ast0.rewrap param
	(match Ast0.unwrap new_id with
	  Ast0.OptIdent(id) ->
	    Ast0.OptDParam(Ast0.rewrap param (Ast0.DParam(id)))
	| _ -> Ast0.DParam(new_id))
  | Ast0.MetaDParamList(name,lenname,cstr,pure) ->
      let arity = all_same true tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_define_param param tgt arity
	(Ast0.MetaDParamList(name,lenname,cstr,pure))
  | Ast0.DPComma(cm) ->
      let arity =
	all_same true tgt (mcode2line cm) [mcode2arity cm] in
      let cm = mcode cm in
      make_define_param param tgt arity (Ast0.DPComma(cm))
  | Ast0.DPdots(dots) ->
      let arity =
	all_same true tgt (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      make_define_param param tgt arity (Ast0.DPdots(dots))
  | Ast0.OptDParam(dp) ->
      failwith "unexpected code"

and fninfo arity = function
    Ast0.FStorage(stg) -> Ast0.FStorage(mcode stg)
  | Ast0.FType(ty) -> Ast0.FType(typeC arity ty)
  | Ast0.FInline(inline) -> Ast0.FInline(mcode inline)
  | Ast0.FAttr(attr) -> Ast0.FAttr(attribute arity attr)

and fninfo2arity fninfo =
  List.concat
    (List.map
       (function
	   Ast0.FStorage(stg) -> [mcode2arity stg]
	 | Ast0.FType(ty) -> []
	 | Ast0.FInline(inline) -> [mcode2arity inline]
	 | Ast0.FAttr(attr) -> [])
       fninfo)

and make_attribute =
  make_opt
    (function x -> failwith "opt not allowed for attributes")

and attribute tgt attr =
  match Ast0.unwrap attr with
    Ast0.Attribute(arg) ->
      let arg = attr_arg tgt arg in
      Ast0.rewrap attr (Ast0.Attribute(arg))

and make_attr_arg =
  make_opt
    (function x -> failwith "opt not allowed for attr_arg")

and attr_arg tgt arg =
  match Ast0.unwrap arg with
    Ast0.AttrName(name) -> Ast0.rewrap arg (Ast0.AttrName(mcode name))
  | Ast0.MetaAttr(name,cstr,pure) ->
      let arity = all_same false tgt (mcode2line name) [mcode2arity name] in
      let name = mcode name in
      make_attr_arg arg tgt arity (Ast0.MetaAttr(name,cstr,pure))

and whencode notfn alwaysfn expression = function
    Ast0.WhenNot (w,e,a) -> Ast0.WhenNot (w,e,notfn a)
  | Ast0.WhenAlways (w,e,a) -> Ast0.WhenAlways (w,e,alwaysfn a)
  | Ast0.WhenModifier(w,x) -> Ast0.WhenModifier(w,x)
  | Ast0.WhenNotTrue (w,e,a) -> Ast0.WhenNotTrue (w,e,expression a)
  | Ast0.WhenNotFalse (w,e,a) -> Ast0.WhenNotFalse (w,e,expression a)

and make_case_line =
  make_opt
    (function x -> Ast0.OptCase x)

and case_line tgt c =
  match Ast0.unwrap c with
    Ast0.Default(def,colon,code) ->
      let arity =
	all_same true tgt (mcode2line def)
	  [mcode2arity def; mcode2arity colon] in
      let def = mcode def in
      let colon = mcode colon in
      let code = dots (statement arity) code in
      make_case_line c tgt arity (Ast0.Default(def,colon,code))
  | Ast0.Case(case,exp,colon,code) ->
      let arity =
	all_same true tgt (mcode2line case)
	  [mcode2arity case; mcode2arity colon] in
      let case = mcode case in
      let exp = expression arity exp in
      let colon = mcode colon in
      let code = dots (statement arity) code in
      make_case_line c tgt arity (Ast0.Case(case,exp,colon,code))
  | Ast0.DisjCase(starter,case_lines,mids,ender) ->
      let case_lines = List.map (case_line tgt) case_lines in
      (match List.rev case_lines with
	_::xs ->
	  if anyopt xs (function Ast0.OptCase(_) -> true | _ -> false)
	  then fail c "opt only allowed in the last disjunct"
      |	_ -> ());
      Ast0.rewrap c (Ast0.DisjCase(starter,case_lines,mids,ender))
  | Ast0.OptCase(_) -> failwith "unexpected OptCase"

and make_exec_code =
  make_opt
    (function x -> failwith "opt not allowed for exec code")

and exec_code tgt e =
  match Ast0.unwrap e with
    Ast0.ExecEval(colon,id) ->
      let arity = all_same false tgt (mcode2line colon) [mcode2arity colon] in
      let colon = mcode colon in
      let id = expression tgt id in
      make_exec_code e tgt arity (Ast0.ExecEval(colon,id))
  | Ast0.ExecToken(tok) ->
      let arity = all_same false tgt (mcode2line tok) [mcode2arity tok] in
      let tok = mcode tok in
      make_exec_code e tgt arity (Ast0.ExecToken(tok))
  | Ast0.ExecDots(dots) ->
      let arity = all_same false tgt (mcode2line dots) [mcode2arity dots] in
      let dots = mcode dots in
      make_exec_code e tgt arity (Ast0.ExecDots(dots))

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
    | Ast0.NONDECL(stmt) ->
	Ast0.NONDECL(statement tgt stmt)
    | Ast0.CODE(rule_elem_dots) ->
	Ast0.CODE(dots (statement tgt) rule_elem_dots)
    | Ast0.TOPCODE(rule_elem_dots) ->  fail t "eliminated by top_level"
    | Ast0.ERRORWORDS(exps) ->
	Ast0.ERRORWORDS(List.map (top_expression false Ast0.NONE) exps)
    | Ast0.OTHER(_) -> fail t "eliminated by top_level")

let rule tgt = List.map (top_level tgt)

(* --------------------------------------------------------------------- *)
(* Entry points *)

let minus_arity code =
  rule Ast0.NONE code
