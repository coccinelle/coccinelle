(*
 * Copyright 2012, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./get_metas.ml"
(* --------------------------------------------------------------------- *)
(* creates AsExpr, etc *)
(* @ attached metavariables can only be associated with positions, so nothing
to do for them *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci

let map_split f l = List.split(List.map f l)

let rewrap x (n,e) = (n,Ast0.rewrap x e)

let mcode x =
  let nonpos l =
    List.filter (function Ast0.MetaPosTag _ -> false | _ -> true) l in
  (nonpos(Ast0.get_pos x),x)

let option_default = []

let bind l1 l2 =
  let oldnames = List.map Ast0.meta_pos_name l2 in
  List.fold_left
    (function prev -> function e1 ->
      if List.mem (Ast0.meta_pos_name e1) oldnames then prev else e1::prev)
    l2 l1

let multibind l =
  let rec loop = function
      [] -> option_default
    | [x] -> x
    | x::xs -> bind x (loop xs) in
  loop l

let map_split_bind f l =
  let (n,e) = List.split(List.map f l) in (multibind n,e)

let get_option f = function
    Some x -> let (n,e) = f x in (n,Some e)
  | None -> (option_default,None)

let do_disj starter lst mids ender processor rebuilder =
  let (starter_n,starter) = mcode starter in
  let (lst_n,lst) = map_split processor lst in
  let (mids_n,mids) = map_split mcode mids in
  let (ender_n,ender) = mcode ender in
  (multibind
     [starter_n;List.hd lst_n;
       multibind (List.map2 bind mids_n (List.tl lst_n));ender_n],
   rebuilder starter lst mids ender)

let dots fn d =
  rewrap d
    (match Ast0.unwrap d with
      Ast0.DOTS(l) ->
	let (n,l) = map_split_bind fn l in (n, Ast0.DOTS(l))
    | Ast0.CIRCLES(l) ->
	let (n,l) = map_split_bind fn l in (n, Ast0.CIRCLES(l))
    | Ast0.STARS(l) ->
	let (n,l) = map_split_bind fn l in (n, Ast0.STARS(l)))
    
let rec ident i =
  let (metas,i) =
  rewrap i
    (match Ast0.unwrap i with
      Ast0.Id(name) ->
	let (n,name) = mcode name in (n,Ast0.Id(name))
    | Ast0.MetaId(name,constraints,seed,pure) ->
	let (n,name) = mcode name in
	(n,Ast0.MetaId(name,constraints,seed,pure))
    | Ast0.MetaFunc(name,constraints,pure) ->
	let (n,name) = mcode name in
	(n,Ast0.MetaFunc(name,constraints,pure))
    | Ast0.MetaLocalFunc(name,constraints,pure) ->
	let (n,name) = mcode name in
	(n,Ast0.MetaLocalFunc(name,constraints,pure))
    | Ast0.AsIdent _ -> failwith "not possible"
    | Ast0.DisjId(starter,id_list,mids,ender) ->
	do_disj starter id_list mids ender ident
	  (fun starter id_list mids ender ->
	    Ast0.DisjId(starter,id_list,mids,ender))
    | Ast0.OptIdent(id) ->
	let (n,id) = ident id in (n,Ast0.OptIdent(id))
    | Ast0.UniqueIdent(id) ->
	let (n,id) = ident id in (n,Ast0.UniqueIdent(id))) in
    List.fold_left
      (function (other_metas,id) ->
	function
	    Ast0.IdentTag(id_meta) ->
	      (other_metas,Ast0.rewrap id (Ast0.AsIdent(id,id_meta)))
	  | x -> (x::other_metas,id))
      ([],i) metas

and expression e =
  let (metas,e) =
    rewrap e
      (match Ast0.unwrap e with
	Ast0.Ident(id) ->
	  let (n,id) = ident id in (n,Ast0.Ident(id))
      | Ast0.Constant(const) ->
	  let (n,const) = mcode const in (n,Ast0.Constant(const))
      | Ast0.StringConstant(lq,str,rq) ->
	  let (lq_n,lq) = mcode lq in
	  let (str_n,str) = dots string_fragment str in
	  let (rq_n,rq) = mcode rq in
	  (multibind [lq_n;str_n;rq_n],Ast0.StringConstant(lq,str,rq))
      | Ast0.FunCall(fn,lp,args,rp) ->
	  let (fn_n,fn) = expression fn in
	  let (lp_n,lp) = mcode lp in
	  let (args_n,args) = dots expression args in
	  let (rp_n,rp) = mcode rp in
	  (multibind [fn_n;lp_n;args_n;rp_n], Ast0.FunCall(fn,lp,args,rp))
      | Ast0.Assignment(left,op,right,simple) ->
	  let (left_n,left) = expression left in
	  let (op_n,op) = mcode op in
	  let (right_n,right) = expression right in
	  (multibind [left_n;op_n;right_n],
	   Ast0.Assignment(left,op,right,simple))
      | Ast0.Sequence(left,op,right) ->
	  let (left_n,left) = expression left in
	  let (op_n,op) = mcode op in
	  let (right_n,right) = expression right in
	  (multibind [left_n;op_n;right_n],
	   Ast0.Sequence(left,op,right))
      | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
	  let (exp1_n,exp1) = expression exp1 in
	  let (why_n,why) = mcode why in
	  let (exp2_n,exp2) = get_option expression exp2 in
	  let (colon_n,colon) = mcode colon in
	  let (exp3_n,exp3) = expression exp3 in
	  (multibind [exp1_n;why_n;exp2_n;colon_n;exp3_n],
	   Ast0.CondExpr(exp1,why,exp2,colon,exp3))
      | Ast0.Postfix(exp,op) ->
	  let (exp_n,exp) = expression exp in
	  let (op_n,op) = mcode op in
	  (bind exp_n op_n, Ast0.Postfix(exp,op))
      | Ast0.Infix(exp,op) ->
	  let (exp_n,exp) = expression exp in
	  let (op_n,op) = mcode op in
	  (bind op_n exp_n, Ast0.Infix(exp,op))
      | Ast0.Unary(exp,op) ->
	  let (exp_n,exp) = expression exp in
	  let (op_n,op) = mcode op in
	  (bind op_n exp_n, Ast0.Unary(exp,op))
      | Ast0.Binary(left,op,right) ->
	  let (left_n,left) = expression left in
	  let (op_n,op) = mcode op in
	  let (right_n,right) = expression right in
	  (multibind [left_n;op_n;right_n], Ast0.Binary(left,op,right))
      | Ast0.Nested(left,op,right) ->
	  let (left_n,left) = expression left in
	  let (op_n,op) = mcode op in
	  let (right_n,right) = expression right in
	  (multibind [left_n;op_n;right_n], Ast0.Nested(left,op,right))
      | Ast0.Paren(lp,exp,rp) ->
	  let (lp_n,lp) = mcode lp in
	  let (exp_n,exp) = expression exp in
	  let (rp_n,rp) = mcode rp in
	  (multibind [lp_n;exp_n;rp_n], Ast0.Paren(lp,exp,rp))
      | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
	  let (exp1_n,exp1) = expression exp1 in
	  let (lb_n,lb) = mcode lb in
	  let (exp2_n,exp2) = expression exp2 in
	  let (rb_n,rb) = mcode rb in
	  (multibind [exp1_n;lb_n;exp2_n;rb_n],
	   Ast0.ArrayAccess(exp1,lb,exp2,rb))
      | Ast0.RecordAccess(exp,pt,field) ->
	  let (exp_n,exp) = expression exp in
	  let (pt_n,pt) = mcode pt in
	  let (field_n,field) = ident field in
	  (multibind [exp_n;pt_n;field_n], Ast0.RecordAccess(exp,pt,field))
      | Ast0.RecordPtAccess(exp,ar,field) ->
	  let (exp_n,exp) = expression exp in
	  let (ar_n,ar) = mcode ar in
	  let (field_n,field) = ident field in
	  (multibind [exp_n;ar_n;field_n], Ast0.RecordPtAccess(exp,ar,field))
      | Ast0.Cast(lp,ty,rp,exp) ->
	  let (lp_n,lp) = mcode lp in
	  let (ty_n,ty) = typeC ty in
	  let (rp_n,rp) = mcode rp in
	  let (exp_n,exp) = expression exp in
	  (multibind [lp_n;ty_n;rp_n;exp_n], Ast0.Cast(lp,ty,rp,exp))
      | Ast0.SizeOfExpr(szf,exp) ->
	  let (szf_n,szf) = mcode szf in
	  let (exp_n,exp) = expression exp in
	  (multibind [szf_n;exp_n],Ast0.SizeOfExpr(szf,exp))
      | Ast0.SizeOfType(szf,lp,ty,rp) ->
	  let (szf_n,szf) = mcode szf in
	  let (lp_n,lp) = mcode lp in
	  let (ty_n,ty) = typeC ty in
          let (rp_n,rp) = mcode rp in
	  (multibind [szf_n;lp_n;ty_n;rp_n], Ast0.SizeOfType(szf,lp,ty,rp))
      | Ast0.TypeExp(ty) ->
	  let (ty_n,ty) = typeC ty in
	  (ty_n,Ast0.TypeExp(ty))
      | Ast0.Constructor(lp,ty,rp,init) ->
	  let (lp_n,lp) = mcode lp in
	  let (ty_n,ty) = typeC ty in
	  let (rp_n,rp) = mcode rp in
	  let (init_n,init) = initialiser init in
	  (multibind [lp_n;ty_n;rp_n;init_n], Ast0.Constructor(lp,ty,rp,init))
      | Ast0.MetaErr(name,constraints,pure) ->
	  let (name_n,name) = mcode name in
	  (name_n,Ast0.MetaErr(name,constraints,pure))
      | Ast0.MetaExpr(name,constraints,ty,form,pure) ->
	  let (name_n,name) = mcode name in
	  (name_n,Ast0.MetaExpr(name,constraints,ty,form,pure))
      | Ast0.MetaExprList(name,lenname,pure) ->
	  let (name_n,name) = mcode name in
	  (name_n,Ast0.MetaExprList(name,lenname,pure))
      |	Ast0.AsExpr _ -> failwith "not possible"
      | Ast0.EComma(cm) ->
	  let (cm_n,cm) = mcode cm in (cm_n,Ast0.EComma(cm))
      | Ast0.DisjExpr(starter,expr_list,mids,ender) ->
	  do_disj starter expr_list mids ender expression
	    (fun starter expr_list mids ender ->
	      Ast0.DisjExpr(starter,expr_list,mids,ender))
      | Ast0.NestExpr(starter,expr_dots,ender,whencode,multi) ->
	  let (starter_n,starter) = mcode starter in
	  let (expr_dots_n,expr_dots) = dots expression expr_dots in
	  let (ender_n,ender) = mcode ender in
	  let (whencode_n,whencode) = get_option expression whencode in
	  (multibind [starter_n;expr_dots_n;ender_n;whencode_n],
	   Ast0.NestExpr(starter,expr_dots,ender,whencode,multi))
      | Ast0.Edots(dots,whencode) ->
	  let (dots_n,dots) = mcode dots in
	  let (whencode_n,whencode) = get_option expression whencode in
	  (bind dots_n whencode_n,Ast0.Edots(dots,whencode))
      | Ast0.Ecircles(dots,whencode) ->
	  let (dots_n,dots) = mcode dots in
	  let (whencode_n,whencode) = get_option expression whencode in
	  (bind dots_n whencode_n,Ast0.Ecircles(dots,whencode))
      | Ast0.Estars(dots,whencode) ->
	  let (dots_n,dots) = mcode dots in
	  let (whencode_n,whencode) = get_option expression whencode in
	  (bind dots_n whencode_n,Ast0.Estars(dots,whencode))
      | Ast0.OptExp(exp) ->
	  let (exp_n,exp) = expression exp in
	  (exp_n,Ast0.OptExp(exp))
      | Ast0.UniqueExp(exp) ->
	  let (exp_n,exp) = expression exp in
	  (exp_n,Ast0.UniqueExp(exp))) in
    List.fold_left
      (function (other_metas,exp) ->
	function
	    Ast0.ExprTag(exp_meta) ->
	      (other_metas,Ast0.rewrap exp (Ast0.AsExpr(exp,exp_meta)))
	  | Ast0.IdentTag(id_meta) ->
	      (other_metas,
	       Ast0.rewrap exp
		 (Ast0.AsExpr(exp,Ast0.rewrap exp (Ast0.Ident(id_meta)))))
	  | x -> (x::other_metas,exp))
      ([],e) metas

and string_fragment e =
  rewrap e
    (match Ast0.unwrap e with
      Ast0.ConstantFragment(str) ->
	let (str_n,str) = mcode str in
	(str_n,Ast0.ConstantFragment(str))
    | Ast0.FormatFragment(pct,fmt) ->
	let (pct_n,pct) = mcode pct in
	let (fmt_n,fmt) = string_format fmt in
	(multibind [pct_n;fmt_n],Ast0.FormatFragment(pct,fmt))
    | Ast0.Strdots(dots) ->
	let (dots_n,dots) = mcode dots in
	(dots_n,Ast0.Strdots(dots))
    | Ast0.MetaFormatList(pct,name,lenname) ->
	let (pct_n,pct) = mcode pct in
	let (name_n,name) = mcode name in
	(bind pct_n name_n,Ast0.MetaFormatList(pct,name,lenname)))

and string_format e =
  rewrap e
    (match Ast0.unwrap e with
      Ast0.ConstantFormat(str) ->
	let (str_n,str) = mcode str in
	(str_n,Ast0.ConstantFormat str)
    | Ast0.MetaFormat(name,constraints) ->
	let (name_n,name) = mcode name in
	(name_n,Ast0.MetaFormat(name,constraints)))

and typeC t =
  let (metas,t) =
    rewrap t
      (match Ast0.unwrap t with
	Ast0.ConstVol(cv,ty) ->
	  let (cv_n,cv) = mcode cv in
	  let (ty_n,ty) = typeC ty in
	  (bind cv_n ty_n, Ast0.ConstVol(cv,ty))
      | Ast0.BaseType(ty,strings) ->
	  let (strings_n,strings) = map_split_bind mcode strings in
	  (strings_n, Ast0.BaseType(ty,strings))
      | Ast0.Signed(sign,ty) ->
	  let (sign_n,sign) = mcode sign in
	  let (ty_n,ty) = get_option typeC ty in
	  (bind sign_n ty_n, Ast0.Signed(sign,ty))
      | Ast0.Pointer(ty,star) ->
	  let (ty_n,ty) = typeC ty in
	  let (star_n,star) = mcode star in
	  (bind ty_n star_n, Ast0.Pointer(ty,star))
      | Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
	  function_pointer (ty,lp1,star,rp1,lp2,params,rp2) []
      | Ast0.FunctionType(ty,lp1,params,rp1) ->
	  function_type (ty,lp1,params,rp1) []
      | Ast0.Array(ty,lb,size,rb) -> array_type (ty,lb,size,rb) []
      |	Ast0.Decimal(dec,lp,length,comma,precision_opt,rp) ->
	  let (dec_n,dec) = mcode dec in
	  let (lp_n,lp) = mcode lp in
	  let (length_n,length) = expression length in
	  let (comma_n,comma) = get_option mcode comma in
	  let (precision_n,precision) =
	    get_option expression precision_opt in
	  let (rp_n,rp) = mcode rp in
	  (multibind [dec_n; lp_n; length_n; comma_n; precision_n; rp_n],
	   Ast0.Decimal(dec,lp,length,comma,precision_opt,rp))
      | Ast0.EnumName(kind,name) ->
	  let (kind_n,kind) = mcode kind in
	  let (name_n,name) = get_option ident name in
	  (bind kind_n name_n, Ast0.EnumName(kind,name))
      | Ast0.EnumDef(ty,lb,ids,rb) ->
	  let (ty_n,ty) = typeC ty in
	  let (lb_n,lb) = mcode lb in
	  let (ids_n,ids) = dots expression ids in
	  let (rb_n,rb) = mcode rb in
	  (multibind [ty_n;lb_n;ids_n;rb_n], Ast0.EnumDef(ty,lb,ids,rb))
      | Ast0.StructUnionName(kind,name) ->
	  let (kind_n,kind) = mcode kind in
	  let (name_n,name) = get_option ident name in
	  (bind kind_n name_n, Ast0.StructUnionName(kind,name))
      | Ast0.StructUnionDef(ty,lb,decls,rb) ->
	  let (ty_n,ty) = typeC ty in
	  let (lb_n,lb) = mcode lb in
	  let (decls_n,decls) = dots declaration decls in
	  let (rb_n,rb) = mcode rb in
	  (multibind [ty_n;lb_n;decls_n;rb_n],
	   Ast0.StructUnionDef(ty,lb,decls,rb))
      | Ast0.TypeName(name) ->
	  let (name_n,name) = mcode name in
	  (name_n,Ast0.TypeName(name))
      | Ast0.MetaType(name,pure) ->
	  let (name_n,name) = mcode name in
	  (name_n,Ast0.MetaType(name,pure))
      |	Ast0.AsType _ -> failwith "not possible"
      | Ast0.DisjType(starter,types,mids,ender) ->
	  do_disj starter types mids ender typeC
	    (fun starter types mids ender ->
	      Ast0.DisjType(starter,types,mids,ender))
      | Ast0.OptType(ty) ->
	  let (ty_n,ty) = typeC ty in (ty_n, Ast0.OptType(ty))
      | Ast0.UniqueType(ty) ->
	  let (ty_n,ty) = typeC ty in (ty_n, Ast0.UniqueType(ty))) in
  List.fold_left
    (function (other_metas,ty) ->
      function
	  Ast0.TypeCTag(ty_meta) ->
	    (other_metas,Ast0.rewrap ty (Ast0.AsType(ty,ty_meta)))
	| x -> (x::other_metas,ty))
    ([],t) metas
    
and function_pointer (ty,lp1,star,rp1,lp2,params,rp2) extra =
  let (ty_n,ty) = typeC ty in
  let (lp1_n,lp1) = mcode lp1 in
  let (star_n,star) = mcode star in
  let (rp1_n,rp1) = mcode rp1 in
  let (lp2_n,lp2) = mcode lp2 in
  let (params_n,params) = dots parameterTypeDef params in
  let (rp2_n,rp2) = mcode rp2 in
    (* have to put the treatment of the identifier into the right position *)
  (multibind ([ty_n;lp1_n;star_n] @ extra @ [rp1_n;lp2_n;params_n;rp2_n]),
   Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2))
and function_type (ty,lp1,params,rp1) extra =
  let (ty_n,ty) = get_option typeC ty in
  let (lp1_n,lp1) = mcode lp1 in
  let (params_n,params) = dots parameterTypeDef params in
  let (rp1_n,rp1) = mcode rp1 in
    (* have to put the treatment of the identifier into the right position *)
  (multibind (ty_n :: extra @ [lp1_n;params_n;rp1_n]),
   Ast0.FunctionType(ty,lp1,params,rp1))
and array_type (ty,lb,size,rb) extra =
  let (ty_n,ty) = typeC ty in
  let (lb_n,lb) = mcode lb in
  let (size_n,size) = get_option expression size in
  let (rb_n,rb) = mcode rb in
  (multibind (ty_n :: extra @ [lb_n;size_n;rb_n]),
   Ast0.Array(ty,lb,size,rb))
    
and named_type ty id =
  let (id_n,id) = ident id in
  match Ast0.unwrap ty with
    Ast0.FunctionPointer(rty,lp1,star,rp1,lp2,params,rp2) ->
      let tyres =
	function_pointer (rty,lp1,star,rp1,lp2,params,rp2) [id_n] in
      (rewrap ty tyres, id)
  | Ast0.FunctionType(rty,lp1,params,rp1) ->
      let tyres = function_type (rty,lp1,params,rp1) [id_n] in
      (rewrap ty tyres, id)
  | Ast0.Array(rty,lb,size,rb) ->
      let tyres = array_type (rty,lb,size,rb) [id_n] in
      (rewrap ty tyres, id)
  | _ -> let (ty_n,ty) = typeC ty in ((bind ty_n id_n, ty), id)
      
and declaration d =
  let (metas,d) =
    rewrap d
      (match Ast0.unwrap d with
	Ast0.MetaDecl(name,pure) ->
	  let (n,name) = mcode name in
	  (n,Ast0.MetaDecl(name,pure))
      | Ast0.MetaField(name,pure) ->
	  let (n,name) = mcode name in
	  (n,Ast0.MetaField(name,pure))
      | Ast0.MetaFieldList(name,lenname,pure) ->
	  let (n,name) = mcode name in
	  (n,Ast0.MetaFieldList(name,lenname,pure))
      |	Ast0.AsDecl _ -> failwith "not possible"
      | Ast0.Init(stg,ty,id,eq,ini,sem) ->
	  let (stg_n,stg) = get_option mcode stg in
	  let ((ty_id_n,ty),id) = named_type ty id in
	  let (eq_n,eq) = mcode eq in
	  let (ini_n,ini) = initialiser ini in
	  let (sem_n,sem) = mcode sem in
	  (multibind [stg_n;ty_id_n;eq_n;ini_n;sem_n],
	   Ast0.Init(stg,ty,id,eq,ini,sem))
      | Ast0.UnInit(stg,ty,id,sem) ->
	  let (stg_n,stg) = get_option mcode stg in
	  let ((ty_id_n,ty),id) = named_type ty id in
	  let (sem_n,sem) = mcode sem in
	  (multibind [stg_n;ty_id_n;sem_n], Ast0.UnInit(stg,ty,id,sem))
      | Ast0.MacroDecl(name,lp,args,rp,sem) ->
	  let (name_n,name) = ident name in
	  let (lp_n,lp) = mcode lp in
	  let (args_n,args) = dots expression args in
	  let (rp_n,rp) = mcode rp in
	  let (sem_n,sem) = mcode sem in
	  (multibind [name_n;lp_n;args_n;rp_n;sem_n],
	   Ast0.MacroDecl(name,lp,args,rp,sem))
      | Ast0.MacroDeclInit(name,lp,args,rp,eq,ini,sem) ->
          let (name_n,name) = ident name in
          let (lp_n,lp) = mcode lp in
          let (args_n,args) = dots expression args in
          let (rp_n,rp) = mcode rp in
          let (eq_n,eq) = mcode eq in
          let (ini_n,ini) = initialiser ini in
          let (sem_n,sem) = mcode sem in
          (multibind [name_n;lp_n;args_n;rp_n;eq_n;ini_n;sem_n],
           Ast0.MacroDeclInit(name,lp,args,rp,eq,ini,sem))
      | Ast0.TyDecl(ty,sem) ->
	  let (ty_n,ty) = typeC ty in
	  let (sem_n,sem) = mcode sem in
	  (bind ty_n sem_n, Ast0.TyDecl(ty,sem))
      | Ast0.Typedef(stg,ty,id,sem) ->
	  let (stg_n,stg) = mcode stg in
	  let (ty_n,ty) = typeC ty in
	  let (id_n,id) = typeC id in
	  let (sem_n,sem) = mcode sem in
	  (multibind [stg_n;ty_n;id_n;sem_n], Ast0.Typedef(stg,ty,id,sem))
      | Ast0.DisjDecl(starter,decls,mids,ender) ->
	  do_disj starter decls mids ender declaration
	    (fun starter decls mids ender ->
	      Ast0.DisjDecl(starter,decls,mids,ender))
      | Ast0.Ddots(dots,whencode) ->
	  let (dots_n,dots) = mcode dots in
	  let (whencode_n,whencode) = get_option declaration whencode in
	  (bind dots_n whencode_n, Ast0.Ddots(dots,whencode))
      | Ast0.OptDecl(decl) ->
	  let (n,decl) = declaration decl in (n,Ast0.OptDecl(decl))
      | Ast0.UniqueDecl(decl) ->
	  let (n,decl) = declaration decl in (n,Ast0.UniqueDecl(decl))) in
  List.fold_left
    (function (other_metas,decl) ->
      function
	  Ast0.DeclTag(decl_meta) ->
	    (other_metas,Ast0.rewrap decl (Ast0.AsDecl(decl,decl_meta)))
	| x -> (x::other_metas,decl))
    ([],d) metas

and initialiser i =
  let (metas,i) =
    rewrap i
      (match Ast0.unwrap i with
	Ast0.MetaInit(name,pure) ->
	  let (name_n,name) = mcode name in
	  (name_n,Ast0.MetaInit(name,pure))
      | Ast0.MetaInitList(name,lenname,pure) ->
	  let (name_n,name) = mcode name in
	  (name_n,Ast0.MetaInitList(name,lenname,pure))
      |	Ast0.AsInit _ -> failwith "not possible"
      | Ast0.InitExpr(exp) ->
	  let (exp_n,exp) = expression exp in
	  (exp_n,Ast0.InitExpr(exp))
      | Ast0.InitList(lb,initlist,rb,ordered) ->
	  let (lb_n,lb) = mcode lb in
	  let (initlist_n,initlist) = dots initialiser initlist in
	  let (rb_n,rb) = mcode rb in
	  (multibind [lb_n;initlist_n;rb_n],
	   Ast0.InitList(lb,initlist,rb,ordered))
      | Ast0.InitGccExt(designators,eq,ini) ->
	  let (dn,designators) = map_split_bind designator designators in
	  let (eq_n,eq) = mcode eq in
	  let (ini_n,ini) = initialiser ini in
	  (multibind [dn;eq_n;ini_n], Ast0.InitGccExt(designators,eq,ini))
      | Ast0.InitGccName(name,eq,ini) ->
	  let (name_n,name) = ident name in
	  let (eq_n,eq) = mcode eq in
	  let (ini_n,ini) = initialiser ini in
	  (multibind [name_n;eq_n;ini_n], Ast0.InitGccName(name,eq,ini))
      | Ast0.IComma(cm) ->
	  let (n,cm) = mcode cm in (n,Ast0.IComma(cm))
      | Ast0.Idots(d,whencode) ->
	  let (d_n,d) = mcode d in
	  let (whencode_n,whencode) = get_option initialiser whencode in
	  (bind d_n whencode_n, Ast0.Idots(d,whencode))
      | Ast0.OptIni(i) ->
	  let (n,i) = initialiser i in (n,Ast0.OptIni(i))
      | Ast0.UniqueIni(i) ->
	  let (n,i) = initialiser i in (n,Ast0.UniqueIni(i))) in
  List.fold_left
    (function (other_metas,init) ->
      function
	  Ast0.InitTag(init_meta) ->
	    (other_metas,Ast0.rewrap init (Ast0.AsInit(init,init_meta)))
	| x -> (x::other_metas,init))
    ([],i) metas

and designator = function
    Ast0.DesignatorField(dot,id) ->
      let (dot_n,dot) = mcode dot in
      let (id_n,id) = ident id in
      (bind dot_n id_n, Ast0.DesignatorField(dot,id))
  | Ast0.DesignatorIndex(lb,exp,rb) ->
      let (lb_n,lb) = mcode lb in
      let (exp_n,exp) = expression exp in
      let (rb_n,rb) = mcode rb in
      (multibind [lb_n;exp_n;rb_n], Ast0.DesignatorIndex(lb,exp,rb))
  | Ast0.DesignatorRange(lb,min,dots,max,rb) ->
      let (lb_n,lb) = mcode lb in
      let (min_n,min) = expression min in
      let (dots_n,dots) = mcode dots in
      let (max_n,max) = expression max in
      let (rb_n,rb) = mcode rb in
      (multibind [lb_n;min_n;dots_n;max_n;rb_n],
       Ast0.DesignatorRange(lb,min,dots,max,rb))
	
and parameterTypeDef p =
  match Ast0.unwrap p with
    Ast0.MetaParamList(name,lenname,pure) ->
      let (metas,p) =
	rewrap p
	  (let (n,name) = mcode name in
	  (n,Ast0.MetaParamList(name,lenname,pure))) in
      List.fold_left
	(function (other_metas,id) ->
	  function
	      ((Ast0.ExprTag(exp_meta)) as x) ->
		(match Ast0.unwrap exp_meta with
		  Ast0.MetaExprList _ ->
		    (other_metas,Ast0.rewrap p (Ast0.AsParam(p,exp_meta)))
		| _ -> (x::other_metas,id))
	    | x -> (x::other_metas,id))
	([],p) metas
  | _ ->
      rewrap p
	(match Ast0.unwrap p with
	  Ast0.VoidParam(ty) ->
	    let (n,ty) = typeC ty in (n,Ast0.VoidParam(ty))
	| Ast0.Param(ty,Some id) ->
	    let ((ty_id_n,ty),id) = named_type ty id in
	    (ty_id_n, Ast0.Param(ty,Some id))
	| Ast0.Param(ty,None) ->
	    let (ty_n,ty) = typeC ty in
	    (ty_n, Ast0.Param(ty,None))
	| Ast0.MetaParam(name,pure) ->
	    let (n,name) = mcode name in
	    (n,Ast0.MetaParam(name,pure))
	| Ast0.MetaParamList(name,lenname,pure) ->
	    failwith "not possible"
	| Ast0.AsParam _ -> failwith "not possible"
	| Ast0.PComma(cm) ->
	    let (n,cm) = mcode cm in (n,Ast0.PComma(cm))
	| Ast0.Pdots(dots) ->
	    let (n,dots) = mcode dots in (n,Ast0.Pdots(dots))
	| Ast0.Pcircles(dots) ->
	    let (n,dots) = mcode dots in (n,Ast0.Pcircles(dots))
	| Ast0.OptParam(param) ->
	    let (n,param) = parameterTypeDef param in (n,Ast0.OptParam(param))
	| Ast0.UniqueParam(param) ->
	    let (n,param) = parameterTypeDef param in
	    (n,Ast0.UniqueParam(param)))
    
and statement s =
  let (metas,s) =
    rewrap s
      (match Ast0.unwrap s with
	Ast0.FunDecl(bef,fi,name,lp,params,rp,lbrace,body,rbrace) ->
	  let (fi_n,fi) = map_split_bind fninfo fi in
	  let (name_n,name) = ident name in
	  let (lp_n,lp) = mcode lp in
	  let (params_n,params) = dots parameterTypeDef params in
	  let (rp_n,rp) = mcode rp in
	  let (lbrace_n,lbrace) = mcode lbrace in
	  let (body_n,body) = dots statement body in
	  let (rbrace_n,rbrace) = mcode rbrace in
	  (multibind
	     [fi_n;name_n;lp_n;params_n;rp_n;lbrace_n;body_n;rbrace_n],
	   Ast0.FunDecl(bef,fi,name,lp,params,rp,lbrace,body,rbrace))
      | Ast0.Decl(bef,decl) ->
	  let (decl_n,decl) = declaration decl in
	  (decl_n,Ast0.Decl(bef,decl))
      | Ast0.Seq(lbrace,body,rbrace) ->
	  let (lbrace_n,lbrace) = mcode lbrace in
	  let (body_n,body) = dots statement body in
	  let (rbrace_n,rbrace) = mcode rbrace in
	  (multibind [lbrace_n;body_n;rbrace_n],
	   Ast0.Seq(lbrace,body,rbrace))
      | Ast0.ExprStatement(exp,sem) ->
	  let (exp_n,exp) = get_option expression exp in
	  let (sem_n,sem) = mcode sem in
	  (bind exp_n sem_n, Ast0.ExprStatement(exp,sem))
      | Ast0.IfThen(iff,lp,exp,rp,branch1,aft) ->
	  let (iff_n,iff) = mcode iff in
	  let (lp_n,lp) = mcode lp in
	  let (exp_n,exp) = expression exp in
	  let (rp_n,rp) = mcode rp in
	  let (branch1_n,branch1) = statement branch1 in
	  (multibind [iff_n;lp_n;exp_n;rp_n;branch1_n],
	   Ast0.IfThen(iff,lp,exp,rp,branch1,aft))
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) ->
	  let (iff_n,iff) = mcode iff in
	  let (lp_n,lp) = mcode lp in
	  let (exp_n,exp) = expression exp in
	  let (rp_n,rp) = mcode rp in
	  let (branch1_n,branch1) = statement branch1 in
	  let (els_n,els) = mcode els in
	  let (branch2_n,branch2) = statement branch2 in
	  (multibind [iff_n;lp_n;exp_n;rp_n;branch1_n;els_n;branch2_n],
	   Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft))
      | Ast0.While(whl,lp,exp,rp,body,aft) ->
	  let (whl_n,whl) = mcode whl in
	  let (lp_n,lp) = mcode lp in
	  let (exp_n,exp) = expression exp in
	  let (rp_n,rp) = mcode rp in
	  let (body_n,body) = statement body in
	  (multibind [whl_n;lp_n;exp_n;rp_n;body_n],
	   Ast0.While(whl,lp,exp,rp,body,aft))
      | Ast0.Do(d,body,whl,lp,exp,rp,sem) ->
	  let (d_n,d) = mcode d in
	  let (body_n,body) = statement body in
	  let (whl_n,whl) = mcode whl in
	  let (lp_n,lp) = mcode lp in
	  let (exp_n,exp) = expression exp in
	  let (rp_n,rp) =  mcode rp in
	  let (sem_n,sem) = mcode sem in
	  (multibind [d_n;body_n;whl_n;lp_n;exp_n;rp_n;sem_n],
	   Ast0.Do(d,body,whl,lp,exp,rp,sem))
      | Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,aft) ->
	  let (fr_n,fr) = mcode fr in
	  let (lp_n,lp) = mcode lp in
	  let (first_n,first) =
	    match Ast0.unwrap first with
	      Ast0.ForExp(e1,sem1) ->
		let (e1_n,e1) = get_option expression e1 in
		let (sem1_n,sem1) = mcode sem1 in
		(bind e1_n sem1_n, Ast0.rewrap first (Ast0.ForExp(e1,sem1)))
	    | Ast0.ForDecl (bef,decl) ->
		let (decl_n,decl) = declaration decl in
		(decl_n,Ast0.rewrap first (Ast0.ForDecl (bef,decl))) in
	  let (e2_n,e2) = get_option expression e2 in
	  let (sem2_n,sem2) = mcode sem2 in
	  let (e3_n,e3) = get_option expression e3 in
	  let (rp_n,rp) = mcode rp in
	  let (body_n,body) = statement body in
	  (multibind [fr_n;lp_n;first_n;e2_n;sem2_n;e3_n;rp_n;body_n],
	   Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,aft))
      | Ast0.Iterator(nm,lp,args,rp,body,aft) ->
	  let (nm_n,nm) = ident nm in
	  let (lp_n,lp) = mcode lp in
	  let (args_n,args) = dots expression args in
	  let (rp_n,rp) = mcode rp in
	  let (body_n,body) = statement body in
	  (multibind [nm_n;lp_n;args_n;rp_n;body_n],
	   Ast0.Iterator(nm,lp,args,rp,body,aft))
      | Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) ->
	  let (switch_n,switch) = mcode switch in
	  let (lp_n,lp) = mcode lp in
	  let (exp_n,exp) = expression exp in
	  let (rp_n,rp) = mcode rp in
	  let (lb_n,lb) = mcode lb in
	  let (decls_n,decls) = dots statement decls in
	  let (cases_n,cases) = dots case_line cases in
	  let (rb_n,rb) = mcode rb in
	  (multibind [switch_n;lp_n;exp_n;rp_n;lb_n;decls_n;cases_n;rb_n],
      	   Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb))
      | Ast0.Break(br,sem) ->
	  let (br_n,br) = mcode br in
	  let (sem_n,sem) = mcode sem in
	  (bind br_n sem_n, Ast0.Break(br,sem))
      | Ast0.Continue(cont,sem) ->
	  let (cont_n,cont) = mcode cont in
	  let (sem_n,sem) = mcode sem in
	  (bind cont_n sem_n, Ast0.Continue(cont,sem))
      | Ast0.Label(l,dd) ->
	  let (l_n,l) = ident l in
	  let (dd_n,dd) = mcode dd in
	  (bind l_n dd_n, Ast0.Label(l,dd))
      | Ast0.Goto(goto,l,sem) ->
	  let (goto_n,goto) = mcode goto in
	  let (l_n,l) = ident l in
	  let (sem_n,sem) = mcode sem in
	  (bind goto_n (bind l_n sem_n), Ast0.Goto(goto,l,sem))
      | Ast0.Return(ret,sem) ->
	  let (ret_n,ret) = mcode ret in
	  let (sem_n,sem) = mcode sem in
	  (bind ret_n sem_n, Ast0.Return(ret,sem))
      | Ast0.ReturnExpr(ret,exp,sem) ->
	  let (ret_n,ret) = mcode ret in
	  let (exp_n,exp) = expression exp in
	  let (sem_n,sem) = mcode sem in
	  (multibind [ret_n;exp_n;sem_n], Ast0.ReturnExpr(ret,exp,sem))
      | Ast0.Exec(exec,lang,code,sem) ->
	  let (exec_n,exec) = mcode exec in
	  let (lang_n,lang) = mcode lang in
	  let (code_n,code) = dots exec_code code in
	  let (sem_n,sem) = mcode sem in
	  (multibind [exec_n;lang_n;code_n;sem_n],
	   Ast0.Exec(exec,lang,code,sem))
      | Ast0.MetaStmt(name,pure) ->
	  let (name_n,name) = mcode name in
	  (name_n,Ast0.MetaStmt(name,pure))
      | Ast0.MetaStmtList(name,pure) ->
	  let (name_n,name) = mcode name in
	  (name_n,Ast0.MetaStmtList(name,pure))
      |	Ast0.AsStmt _ -> failwith "not possible"
      | Ast0.Disj(starter,statement_dots_list,mids,ender) ->
	  do_disj starter statement_dots_list mids ender (dots statement)
	    (fun starter statement_dots_list mids ender ->
	      Ast0.Disj(starter,statement_dots_list,mids,ender))
      | Ast0.Nest(starter,stmt_dots,ender,whn,multi) ->
	  let (starter_n,starter) = mcode starter in
	  let (stmt_dots_n,stmt_dots) = dots statement stmt_dots in
	  let (ender_n,ender) = mcode ender in
	  let (whn_n,whn) =
	    map_split_bind (whencode (dots statement) statement) whn in
	  (multibind [starter_n;stmt_dots_n;ender_n;whn_n],
	   Ast0.Nest(starter,stmt_dots,ender,whn,multi))
      | Ast0.Exp(exp) ->
	  let (exp_n,exp) = expression exp in
	  (exp_n,Ast0.Exp(exp))
      | Ast0.TopExp(exp) ->
	  let (exp_n,exp) = expression exp in
	  (exp_n,Ast0.TopExp(exp))
      | Ast0.Ty(ty) ->
	  let (ty_n,ty) = typeC ty in
	  (ty_n,Ast0.Ty(ty))
      | Ast0.TopInit(init) ->
	  let (init_n,init) = initialiser init in
	  (init_n,Ast0.TopInit(init))
      | Ast0.Dots(d,whn) ->
	  let (d_n,d) = mcode d in
	  let (whn_n,whn) =
	    map_split_bind (whencode (dots statement) statement) whn in
	  (bind d_n whn_n, Ast0.Dots(d,whn))
      | Ast0.Circles(d,whn) ->
	  let (d_n,d) = mcode d in
	  let (whn_n,whn) =
	    map_split_bind (whencode (dots statement) statement) whn in
	  (bind d_n whn_n, Ast0.Circles(d,whn))
      | Ast0.Stars(d,whn) ->
	  let (d_n,d) = mcode d in
	  let (whn_n,whn) =
	    map_split_bind (whencode (dots statement) statement) whn in
	  (bind d_n whn_n, Ast0.Stars(d,whn))
      | Ast0.Include(inc,name) ->
	  let (inc_n,inc) = mcode inc in
	  let (name_n,name) = mcode name in
	  (bind inc_n name_n, Ast0.Include(inc,name))
      | Ast0.Undef(def,id) ->
	  let (def_n,def) = mcode def in
	  let (id_n,id) = ident id in
	  (multibind [def_n;id_n],Ast0.Undef(def,id))
      | Ast0.Define(def,id,params,body) ->
	  let (def_n,def) = mcode def in
	  let (id_n,id) = ident id in
	  let (params_n,params) = define_parameters params in
	  let (body_n,body) = dots statement body in
	  (multibind [def_n;id_n;params_n;body_n],
	   Ast0.Define(def,id,params,body))
      | Ast0.Pragma(prg,id,body) ->
	  let (prg_n,prg) = mcode prg in
	  let (id_n,id) = ident id in
	  let (body_n,body) = pragmainfo body in
	  (multibind [prg_n;id_n;body_n],Ast0.Pragma(prg,id,body))
      | Ast0.OptStm(re) ->
	  let (re_n,re) = statement re in (re_n,Ast0.OptStm(re))
      | Ast0.UniqueStm(re) ->
	  let (re_n,re) = statement re in (re_n,Ast0.UniqueStm(re))) in
  List.fold_left
    (function (other_metas,stmt) ->
      function
	  Ast0.StmtTag(stmt_meta) ->
	    (other_metas,Ast0.rewrap stmt (Ast0.AsStmt(stmt,stmt_meta)))
	| x -> (x::other_metas,stmt))
    ([],s) metas

and pragmainfo pi =
  rewrap pi
    (match Ast0.unwrap pi with
      Ast0.PragmaTuple(lp,args,rp) ->
	let (lp_n,lp) = mcode lp in
	let (args_n,args) = dots expression args in
	let (rp_n,rp) = mcode rp in
	(multibind [lp_n;args_n;rp_n], Ast0.PragmaTuple(lp,args,rp))
    | Ast0.PragmaIdList(ids) ->
	let (ids_n,ids) = dots ident ids in
	(ids_n, Ast0.PragmaIdList(ids))
    | Ast0.PragmaDots (dots) ->
	let (dots_n,dots) = mcode dots in
	(dots_n,Ast0.PragmaDots dots))
    
  (* not parameterizable for now... *)
and define_parameters p =
  rewrap p
    (match Ast0.unwrap p with
      Ast0.NoParams -> (option_default,Ast0.NoParams)
    | Ast0.DParams(lp,params,rp) ->
	let (lp_n,lp) = mcode lp in
	let (params_n,params) = dots define_param params in
	let (rp_n,rp) = mcode rp in
	(multibind [lp_n;params_n;rp_n], Ast0.DParams(lp,params,rp)))
    
and define_param p =
  rewrap p
    (match Ast0.unwrap p with
      Ast0.DParam(id) -> let (n,id) = ident id in (n,Ast0.DParam(id))
    | Ast0.DPComma(comma) ->
	let (n,comma) = mcode comma in (n,Ast0.DPComma(comma))
    | Ast0.DPdots(d) ->
	let (n,d) = mcode d in (n,Ast0.DPdots(d))
    | Ast0.DPcircles(c) ->
	let (n,c) = mcode c in (n,Ast0.DPcircles(c))
    | Ast0.OptDParam(dp) ->
	let (n,dp) = define_param dp in (n,Ast0.OptDParam(dp))
    | Ast0.UniqueDParam(dp) ->
	let (n,dp) = define_param dp in (n,Ast0.UniqueDParam(dp)))
    
and fninfo = function
    Ast0.FStorage(stg) ->
      let (n,stg) = mcode stg in (n,Ast0.FStorage(stg))
  | Ast0.FType(ty) -> let (n,ty) = typeC ty in (n,Ast0.FType(ty))
  | Ast0.FInline(inline) ->
      let (n,inline) = mcode inline in (n,Ast0.FInline(inline))
  | Ast0.FAttr(init) ->
      let (n,init) = mcode init in (n,Ast0.FAttr(init))
	
and whencode notfn alwaysfn = function
    Ast0.WhenNot a -> let (n,a) = notfn a in (n,Ast0.WhenNot(a))
  | Ast0.WhenAlways a -> let (n,a) = alwaysfn a in (n,Ast0.WhenAlways(a))
  | Ast0.WhenModifier(x) -> (option_default,Ast0.WhenModifier(x))
  | Ast0.WhenNotTrue(e) ->
      let (n,e) = expression e in (n,Ast0.WhenNotTrue(e))
  | Ast0.WhenNotFalse(e) ->
      let (n,e) = expression e in (n,Ast0.WhenNotFalse(e))
	
and case_line c =
  rewrap c
    (match Ast0.unwrap c with
      Ast0.Default(def,colon,code) ->
	let (def_n,def) = mcode def in
	let (colon_n,colon) = mcode colon in
	let (code_n,code) = dots statement code in
	(multibind [def_n;colon_n;code_n], Ast0.Default(def,colon,code))
    | Ast0.Case(case,exp,colon,code) ->
	let (case_n,case) = mcode case in
	let (exp_n,exp) = expression exp in
	let (colon_n,colon) = mcode colon in
	let (code_n,code) = dots statement code in
	(multibind [case_n;exp_n;colon_n;code_n],
	 Ast0.Case(case,exp,colon,code))
    | Ast0.DisjCase(starter,case_lines,mids,ender) ->
	do_disj starter case_lines mids ender case_line
	  (fun starter case_lines mids ender ->
	    Ast0.DisjCase(starter,case_lines,mids,ender))
    | Ast0.OptCase(case) ->
	let (n,case) = case_line case in (n,Ast0.OptCase(case)))

and exec_code e =
  rewrap e
    (match Ast0.unwrap e with
      Ast0.ExecEval(colon,id) ->
	let (colon_n,colon) = mcode colon in
	let (id_n,id) = expression id in
	(multibind [colon_n;id_n], Ast0.ExecEval(colon,id))
    | Ast0.ExecToken(tok) ->
	let (tok_n,tok) = mcode tok in
	(tok_n,Ast0.ExecToken(tok))
    | Ast0.ExecDots(dots) ->
	let (dots_n,dots) = mcode dots in
	(dots_n,Ast0.ExecDots(dots)))
    
and top_level t =
  rewrap t
    (match Ast0.unwrap t with
      Ast0.FILEINFO(old_file,new_file) ->
	let (old_file_n,old_file) = mcode old_file in
	let (new_file_n,new_file) = mcode new_file in
	(bind old_file_n new_file_n,Ast0.FILEINFO(old_file,new_file))
    | Ast0.NONDECL(statement_dots) ->
	let (n,statement_dots) = statement statement_dots in
	(n,Ast0.NONDECL(statement_dots))
    | Ast0.CODE(stmt_dots) ->
	let (stmt_dots_n,stmt_dots) = dots statement stmt_dots in
	(stmt_dots_n, Ast0.CODE(stmt_dots))
    | Ast0.TOPCODE(stmt_dots) ->
	let (stmt_dots_n,stmt_dots) = dots statement stmt_dots in
	(stmt_dots_n, Ast0.TOPCODE(stmt_dots))
    | Ast0.ERRORWORDS(exps) ->
	let (n,exps) = map_split_bind expression exps in
	(n, Ast0.ERRORWORDS(exps))
    | Ast0.OTHER(_) -> failwith "unexpected code")
    
let process t =
  List.map
    (function x ->
      match top_level x with
	([],code) -> code
      |	(l,_) ->
	  failwith
	    (Printf.sprintf
	       "rule starting on line %d contains unattached metavariables: %s"
	       (Ast0.get_line x)
	       (String.concat ", "
		  (List.map
		     (function nm ->
		       let (r,n) = Ast0.unwrap_mcode nm in r^"."^n)
		     (List.map Ast0.meta_pos_name l)))))
    t
