(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

(* --------------------------------------------------------------------- *)
(* Generic traversal: rebuilder *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module VT0 = Visitor_ast0_types

type mode = COMBINER | REBUILDER | BOTH

let map_split f l = List.split(List.map f l)

let rewrap x (n,e) = (n,Ast0.rewrap x e)

let visitor mode bind option_default
    meta_mcode string_mcode const_mcode simpleAssign_mcode opAssign_mcode
    fix_mcode unary_mcode arithOp_mcode logicalOp_mcode cv_mcode sign_mcode
    struct_mcode storage_mcode inc_mcode
    dotsexprfn dotsinitfn dotsparamfn dotstemplateparamfn dotsstmtfn dotsdeclfn dotsfieldfn
    dotsenumdeclfn dotscasefn dotsdefparfn
    identfn exprfn assignOpfn binaryOpfn tyfn initfn paramfn template_paramfn declfn fieldfn
    enumdeclfn
    stmtfn forinfofn casefn string_fragmentfn attributefn attr_argfn topfn =
  let multibind l =
    let rec loop = function
	[] -> option_default
      |	[x] -> x
      |	x::xs -> bind x (loop xs) in
    loop l in
  let map_split_bind f l =
    let (n,e) = List.split(List.map f l) in (multibind n,e) in
  let get_option f = function
      Some x -> let (n,e) = f x in (n,Some e)
    | None -> (option_default,None) in
  let do_disj starter lst mids ender processor rebuilder = (* disj and conj *)
    let (starter_n,starter) = string_mcode starter in
    (* slightly ugly but ensures correct evaluation order. *)
    let (first_n, first) = processor (List.hd lst) in
    let rest = List.map2 (fun m l ->
      let m1 = string_mcode m in
      let p1 = processor l in (m1, p1)) mids (List.tl lst) in
    let bind_value = first_n ::
      List.rev(
        List.fold_left (fun acc ((m1,_),(p1,_)) -> p1 :: m1 :: acc) [] rest) in
    let lst = first :: (List.map (fun (_,(_,n)) -> n) rest) in
    let mids = List.map (fun ((_,n),_) -> n) rest in
    let (ender_n,ender) = string_mcode ender in
    (multibind [starter_n; multibind bind_value;ender_n],
     rebuilder starter lst mids ender) in
  let dotsfn param default all_functions arg =
    let k d = rewrap d (map_split_bind default (Ast0.unwrap d)) in
    param all_functions k arg in
  let strdotsfn all_functions k arg = k arg in
  let ecdotsfn all_functions k arg = k arg in

  let rec expression_dots d = dotsfn dotsexprfn expression all_functions d
  and initialiser_dots d = dotsfn dotsinitfn initialiser all_functions d
  and parameter_dots d = dotsfn dotsparamfn parameterTypeDef all_functions d
  and template_parameter_dots d = dotsfn dotstemplateparamfn templateParameterTypeDef all_functions d
  and statement_dots d = dotsfn dotsstmtfn statement all_functions d
  and declaration_dots d = dotsfn dotsdeclfn declaration all_functions d
  and field_dots d = dotsfn dotsfieldfn field all_functions d
  and enum_decl_dots d = dotsfn dotsenumdeclfn enum_decl all_functions d
  and case_line_dots d = dotsfn dotscasefn case_line all_functions d
  and string_fragment_dots d = dotsfn strdotsfn string_fragment all_functions d
  and exec_code_dots d = dotsfn ecdotsfn exec_code all_functions d
  and define_param_dots d = dotsfn dotsdefparfn define_param all_functions d

  and ident i =
    let k i =
      rewrap i
	(match Ast0.unwrap i with
	  Ast0.Id(name) ->
	    let (n,name) = string_mcode name in (n,Ast0.Id(name))
	| Ast0.MetaId(name,constraints,seed,pure) ->
	    let (n,name) = meta_mcode name in
	    (n,Ast0.MetaId(name,constraints,seed,pure))
	| Ast0.MetaFunc(name,constraints,pure) ->
	    let (n,name) = meta_mcode name in
	    (n,Ast0.MetaFunc(name,constraints,pure))
	| Ast0.MetaLocalFunc(name,constraints,pure) ->
	    let (n,name) = meta_mcode name in
	    (n,Ast0.MetaLocalFunc(name,constraints,pure))
	| Ast0.DisjId(starter,id_list,mids,ender) ->
	    do_disj starter id_list mids ender ident
	      (fun starter id_list mids ender ->
		Ast0.DisjId(starter,id_list,mids,ender))
	| Ast0.ConjId(starter,id_list,mids,ender) ->
	    do_disj starter id_list mids ender ident
	      (fun starter id_list mids ender ->
		Ast0.ConjId(starter,id_list,mids,ender))
	| Ast0.OptIdent(id) ->
	    let (n,id) = ident id in (n,Ast0.OptIdent(id))
	| Ast0.AsIdent(id,asid) ->
	    let (id_n,id) = ident id in
	    let (asid_n,asid) = ident asid in
	    (bind id_n asid_n, Ast0.AsIdent(id,asid))) in
    identfn all_functions k i

  and expression e =
    let k e =
      rewrap e
	(match Ast0.unwrap e with
	  Ast0.Ident(id) ->
	    let (n,id) = ident id in (n,Ast0.Ident(id))
	| Ast0.Constant(const) ->
	    let (n,const) = const_mcode const in (n,Ast0.Constant(const))
	| Ast0.StringConstant(lq,str,rq,sz) ->
	    let (lq_n,lq) = string_mcode lq in
	    let (str_n,str) = string_fragment_dots str in
	    let (rq_n,rq) = string_mcode rq in
	    (multibind [lq_n;str_n;rq_n],Ast0.StringConstant(lq,str,rq,sz))
	| Ast0.FunCall(fn,lp,args,rp) ->
	    let (fn_n,fn) = expression fn in
	    let (lp_n,lp) = string_mcode lp in
	    let (args_n,args) = expression_dots args in
	    let (rp_n,rp) = string_mcode rp in
	    (multibind [fn_n;lp_n;args_n;rp_n], Ast0.FunCall(fn,lp,args,rp))
	| Ast0.Assignment(left,op,right,simple) ->
	    let (left_n,left) = expression left in
	    let (op_n,op) = assignOp op in
	    let (right_n,right) = expression right in
	    (multibind [left_n;op_n;right_n],
	     Ast0.Assignment(left,op,right,simple))
	| Ast0.Sequence(left,op,right) ->
	    let (left_n,left) = expression left in
	    let (op_n,op) = string_mcode op in
	    let (right_n,right) = expression right in
	    (multibind [left_n;op_n;right_n],
	     Ast0.Sequence(left,op,right))
	| Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
	    let (exp1_n,exp1) = expression exp1 in
	    let (why_n,why) = string_mcode why in
	    let (exp2_n,exp2) = get_option expression exp2 in
	    let (colon_n,colon) = string_mcode colon in
	    let (exp3_n,exp3) = expression exp3 in
	    (multibind [exp1_n;why_n;exp2_n;colon_n;exp3_n],
	     Ast0.CondExpr(exp1,why,exp2,colon,exp3))
	| Ast0.Postfix(exp,op) ->
	    let (exp_n,exp) = expression exp in
	    let (op_n,op) = fix_mcode op in
	    (bind exp_n op_n, Ast0.Postfix(exp,op))
	| Ast0.Infix(exp,op) ->
	    let (op_n,op) = fix_mcode op in
	    let (exp_n,exp) = expression exp in
	    (bind op_n exp_n, Ast0.Infix(exp,op))
	| Ast0.Unary(exp,op) ->
	    let (op_n,op) = unary_mcode op in
	    let (exp_n,exp) = expression exp in
	    (bind op_n exp_n, Ast0.Unary(exp,op))
	| Ast0.Binary(left,op,right) ->
	    let (left_n,left) = expression left in
	    let (op_n,op) = binaryOp op in
	    let (right_n,right) = expression right in
	    (multibind [left_n;op_n;right_n], Ast0.Binary(left,op,right))
	| Ast0.Nested(left,op,right) ->
	    let (left_n,left) = expression left in
	    let (op_n,op) = binaryOp op in
	    let (right_n,right) = expression right in
	    (multibind [left_n;op_n;right_n], Ast0.Nested(left,op,right))
	| Ast0.Paren(lp,exp,rp) ->
	    let (lp_n,lp) = string_mcode lp in
	    let (exp_n,exp) = expression exp in
	    let (rp_n,rp) = string_mcode rp in
	    (multibind [lp_n;exp_n;rp_n], Ast0.Paren(lp,exp,rp))
	| Ast0.ArrayAccess(exp,lb,args,rb) ->
	    let (exp_n,exp) = expression exp in
	    let (lb_n,lb) = string_mcode lb in
	    let (args_n,args) = expression_dots args in
	    let (rb_n,rb) = string_mcode rb in
	    (multibind [exp_n;lb_n;args_n;rb_n], Ast0.ArrayAccess(exp,lb,args,rb))
	| Ast0.RecordAccess(exp,pt,field) ->
	    let (exp_n,exp) = expression exp in
	    let (pt_n,pt) = string_mcode pt in
	    let (field_n,field) = ident field in
	    (multibind [exp_n;pt_n;field_n], Ast0.RecordAccess(exp,pt,field))
	| Ast0.RecordPtAccess(exp,ar,field) ->
	    let (exp_n,exp) = expression exp in
	    let (ar_n,ar) = string_mcode ar in
	    let (field_n,field) = ident field in
	    (multibind [exp_n;ar_n;field_n], Ast0.RecordPtAccess(exp,ar,field))
        | Ast0.QualifiedAccess(ty,coloncolon,field) ->
            let (ty_n,ty) = get_option typeC ty in
            let (coloncolon_n,coloncolon) = string_mcode coloncolon in
            let (field_n,field) = ident field in
            (multibind [ty_n;coloncolon_n;field_n], Ast0.QualifiedAccess(ty,coloncolon,field))
	| Ast0.Cast(lp,ty,rp,exp) ->
	    let (lp_n,lp) = string_mcode lp in
	    let (ty_n,ty) = typeC ty in
	    let (rp_n,rp) = string_mcode rp in
	    let (exp_n,exp) = expression exp in
            (multibind [lp_n;ty_n;rp_n;exp_n],
             Ast0.Cast(lp,ty,rp,exp))
	| Ast0.SizeOfExpr(szf,exp) ->
	    let (szf_n,szf) = string_mcode szf in
	    let (exp_n,exp) = expression exp in
	    (multibind [szf_n;exp_n],Ast0.SizeOfExpr(szf,exp))
	| Ast0.SizeOfType(szf,lp,ty,rp) ->
	    let (szf_n,szf) = string_mcode szf in
	    let (lp_n,lp) = string_mcode lp in
	    let (ty_n,ty) = typeC ty in
            let (rp_n,rp) = string_mcode rp in
	    (multibind [szf_n;lp_n;ty_n;rp_n], Ast0.SizeOfType(szf,lp,ty,rp))
	| Ast0.CoAwaitYield(ret,exp) ->
	    let (ret_n,ret) = string_mcode ret in
	    let (exp_n,exp) = expression exp in
	    (multibind [ret_n;exp_n], Ast0.CoAwaitYield(ret,exp))
	| Ast0.Delete(dlt, exp) ->
	    let (dlt_n,dlt) = string_mcode dlt in
	    let (exp_n,exp) = expression exp in
	    (multibind [dlt_n;exp_n],Ast0.Delete(dlt,exp))
	| Ast0.DeleteArr(dlt,lb,rb,exp) ->
	    let (dlt_n,dlt) = string_mcode dlt in
	    let (lb_n,lb) = string_mcode lb in
            let (rb_n,rb) = string_mcode rb in
	    let (exp_n,exp) = expression exp in
	    (multibind [dlt_n;lb_n;rb_n;exp_n], Ast0.DeleteArr(dlt,lb,rb,exp))
	| Ast0.New(nw,pp_opt,lp2_opt,ty,rp2_opt,args_opt) ->
	    let (nw_n,nw) = string_mcode nw in
	    let (pp_opt_n, pp_opt) = get_option argslist pp_opt in
	    let (lp2_n,lp2) = get_option string_mcode lp2_opt in
	    let (ty_n,ty) = typeC ty in
	    let (rp2_n,rp2) = get_option string_mcode rp2_opt in
	    let (args_n, args) = get_option argslist args_opt in
	    (multibind [nw_n;pp_opt_n;lp2_n;ty_n;rp2_n;args_n], Ast0.New(nw,pp_opt,lp2,ty,rp2,args_opt))
	| Ast0.TemplateInst(tn,lp,args,rp) ->
	    let (tn_n,tn) = expression tn in
	    let (lp_n,lp) = string_mcode lp in
	    let (args_n,args) = expression_dots args in
	    let (rp_n,rp) = string_mcode rp in
	    (multibind [tn_n;lp_n;args_n;rp_n], Ast0.TemplateInst(tn,lp,args,rp))
        | Ast0.TupleExpr(init) ->
            let (init_n,init) = initialiser init in
            (init_n, Ast0.TupleExpr(init))
	| Ast0.TypeExp(ty) ->
	    let (ty_n,ty) = typeC ty in
	    (ty_n,Ast0.TypeExp(ty))
	| Ast0.Constructor(lp,ty,rp,init) ->
	    let (lp_n,lp) = string_mcode lp in
	    let (ty_n,ty) = typeC ty in
	    let (rp_n,rp) = string_mcode rp in
	    let (init_n,init) = initialiser init in
	    (multibind [lp_n;ty_n;rp_n;init_n], Ast0.Constructor(lp,ty,rp,init))
	| Ast0.MetaErr(name,constraints,pure) ->
	    let (name_n,name) = meta_mcode name in
	    (name_n,Ast0.MetaErr(name,constraints,pure))
	| Ast0.MetaExpr(name,constraints,ty,form,pure,bitfield) ->
	    let (name_n,name) = meta_mcode name in
	    (name_n,Ast0.MetaExpr(name,constraints,ty,form,pure,bitfield))
	| Ast0.MetaExprList(name,lenname,constraints,pure) ->
	    let (name_n,name) = meta_mcode name in
	    (name_n,Ast0.MetaExprList(name,lenname,constraints,pure))
	| Ast0.EComma(cm) ->
	    let (cm_n,cm) = string_mcode cm in (cm_n,Ast0.EComma(cm))
	| Ast0.DisjExpr(starter,expr_list,mids,ender) ->
	    do_disj starter expr_list mids ender expression
	      (fun starter expr_list mids ender ->
		Ast0.DisjExpr(starter,expr_list,mids,ender))
	| Ast0.ConjExpr(starter,expr_list,mids,ender) ->
	    do_disj starter expr_list mids ender expression
	      (fun starter expr_list mids ender ->
		Ast0.ConjExpr(starter,expr_list,mids,ender))
	| Ast0.NestExpr(starter,expr_dots,ender,whencode,multi) ->
	    let (starter_n,starter) = string_mcode starter in
	    let (whencode_n, whencode) = whencode_option expression whencode in
	    let (expr_dots_n,expr_dots) = expression_dots expr_dots in
	    let (ender_n,ender) = string_mcode ender in
	    (multibind [starter_n;expr_dots_n;ender_n;whencode_n],
	     Ast0.NestExpr(starter,expr_dots,ender,whencode,multi))
	| Ast0.Edots(dots,whencode) ->
	    let (dots_n,dots) = string_mcode dots in
	    let (whencode_n, whencode) = whencode_option expression whencode in
	    (bind dots_n whencode_n,Ast0.Edots(dots,whencode))
	| Ast0.OptExp(exp) ->
	    let (exp_n,exp) = expression exp in
	    (exp_n,Ast0.OptExp(exp))
	| Ast0.AsExpr(exp,asexp) ->
	    let (exp_n,exp) = expression exp in
	    let (asexp_n,asexp) = expression asexp in
	    (bind exp_n asexp_n, Ast0.AsExpr(exp,asexp))
	| Ast0.AsSExpr(exp,asstm) ->
	    let (exp_n,exp) = expression exp in
	    let (asstm_n,asstm) = statement asstm in
	    (bind exp_n asstm_n, Ast0.AsSExpr(exp,asstm))) in
    exprfn all_functions k e
  and argslist (lp,args,rp) =
    let (lp_n,lp) = string_mcode lp in
    let (args_n, args) = expression_dots args in
    let (rp_n,rp) = string_mcode rp in
    (multibind [lp_n; args_n; rp_n], (lp,args,rp))
  and string_fragment e =
    let k e =
      rewrap e
	(match Ast0.unwrap e with
	  Ast0.ConstantFragment(str) ->
	    let (str_n,str) = string_mcode str in
	    (str_n,Ast0.ConstantFragment(str))
	| Ast0.FormatFragment(pct,fmt) ->
	    let (pct_n,pct) = string_mcode pct in
	    let (fmt_n,fmt) = string_format fmt in
	    (multibind [pct_n;fmt_n], Ast0.FormatFragment(pct,fmt))
	| Ast0.Strdots dots ->
	    let (dots_n,dots) = string_mcode dots in
	    (dots_n,Ast0.Strdots dots)
	| Ast0.MetaFormatList(pct,name,cstr,lenname) ->
	    let (pct_n,pct) = string_mcode pct in
	    let (name_n,name) = meta_mcode name in
	    (bind pct_n name_n,Ast0.MetaFormatList(pct,name,cstr,lenname))) in
    string_fragmentfn all_functions k e

  and string_format e =
    let k e =
      rewrap e
	(match Ast0.unwrap e with
	  Ast0.ConstantFormat(str) ->
	    let (str_n,str) = string_mcode str in
	    (str_n,Ast0.ConstantFormat str)
	| Ast0.MetaFormat(name,constraints) ->
	    let (name_n,name) = meta_mcode name in
	    (name_n,Ast0.MetaFormat(name,constraints))) in
    k e

  and assignOp e =
    let k e =
      rewrap e
        (match Ast0.unwrap e with
           Ast0.SimpleAssign o ->
           let (o_n,o) = simpleAssign_mcode o in
           (o_n, Ast0.SimpleAssign o)
         | Ast0.OpAssign o ->
            let (o_n,o) = opAssign_mcode o in
            (o_n, Ast0.OpAssign o)
        | Ast0.MetaAssign (name, c, pure) ->
          let (name_n,name) = meta_mcode name in
          (name_n,Ast0.MetaAssign(name,c, pure))) in
    assignOpfn all_functions k e

  and binaryOp e =
    let k e =
      rewrap e
        (match Ast0.unwrap e with
           Ast0.Arith o ->
             let (o_n,o) = arithOp_mcode o in
             (o_n,Ast0.Arith o)
         | Ast0.Logical o ->
            let (o_n, o) = logicalOp_mcode o in
            (o_n, Ast0.Logical o)
        | Ast0.MetaBinary (name, c, pure) ->
          let (name_n,name) = meta_mcode name in
          (name_n,Ast0.MetaBinary(name, c, pure))) in
    binaryOpfn all_functions k e

  and typeC t =
    let k t =
      rewrap t
	(match Ast0.unwrap t with
	  Ast0.ConstVol(cvbefore,ty,cvafter) ->
	    let do_cvattr = function
		Ast0.CV cv ->
		  let (cv_n,cv) = cv_mcode cv in
		  (cv_n,Ast0.CV cv)
	      | Ast0.Attr attr ->
		  let (attr_n,attr) = attribute attr in
		  (attr_n,Ast0.Attr attr) in
	    let (cvbefore_n,cvbefore) =
	      map_split_bind do_cvattr cvbefore in
	    let (ty_n,ty) = typeC ty in
	    let (cvafter_n,cvafter) =
	      map_split_bind do_cvattr cvafter in
	    let front = multibind (cvbefore_n::ty_n::[cvafter_n]) in
	    (front, Ast0.ConstVol(cvbefore,ty,cvafter))
	| Ast0.BaseType(ty,strings) ->
	    let (strings_n,strings) = map_split_bind string_mcode strings in
	    (strings_n, Ast0.BaseType(ty,strings))
	| Ast0.Signed(sign,ty) ->
	    let (sign_n,sign) = sign_mcode sign in
	    let (ty_n,ty) = get_option typeC ty in
	    (bind sign_n ty_n, Ast0.Signed(sign,ty))
	| Ast0.Pointer(ty,star) ->
	    let (ty_n,ty) = typeC ty in
	    let (star_n,star) = unary_mcode star in
	    (bind ty_n star_n, Ast0.Pointer(ty,star))
        | Ast0.ParenType(lp,ty,rp) ->
	    let (t,id) =
              parentype_type (lp,ty,None,rp) in t
        | Ast0.FunctionType(ty,lp,params,rp) ->
	    let (t,id) =
              functiontype_type (ty,None,lp,params,rp) in t
	| Ast0.Array(ty,lb,size,rb) ->
            let (t,id) = array_type (ty,None,lb,size,rb) in t
	| Ast0.Decimal(dec,lp,length,comma,precision_opt,rp) ->
	    let (dec_n,dec) = string_mcode dec in
	    let (lp_n,lp) = string_mcode lp in
	    let (length_n,length) = expression length in
	    let (comma_n,comma) = get_option string_mcode comma in
	    let (precision_n,precision) =
	      get_option expression precision_opt in
	    let (rp_n,rp) = string_mcode rp in
	    (multibind [dec_n; lp_n; length_n; comma_n; precision_n; rp_n],
	     Ast0.Decimal(dec,lp,length,comma,precision_opt,rp))
	| Ast0.EnumName(kind,key,name) ->
	    let (kind_n,kind) = string_mcode kind in
	    let (key_n, key) = get_option struct_mcode key in
	    let (name_n,name) = get_option ident name in
	    (multibind [kind_n;key_n;name_n], Ast0.EnumName(kind,key,name))
	| Ast0.EnumDef(ty,base,lb,ids,rb) ->
	    let (ty_n,ty) = typeC ty in
	    let (base_n, base) = get_option enum_base base in
	    let (lb_n,lb) = string_mcode lb in
	    let (ids_n,ids) = enum_decl_dots ids in
	    let (rb_n,rb) = string_mcode rb in
	    (multibind [ty_n;lb_n;ids_n;rb_n], Ast0.EnumDef(ty,base,lb,ids,rb))
	| Ast0.StructUnionName(kind,name) ->
	    let (kind_n,kind) = struct_mcode kind in
	    let (name_n,name) = get_option ident name in
	    (bind kind_n name_n, Ast0.StructUnionName(kind,name))
	| Ast0.StructUnionDef(ty,lb,decls,rb) ->
	    let (ty_n,ty) = typeC ty in
	    let (lb_n,lb) = string_mcode lb in
	    let (decls_n,decls) = field_dots decls in
	    let (rb_n,rb) = string_mcode rb in
	    (multibind [ty_n;lb_n;decls_n;rb_n],
	     Ast0.StructUnionDef(ty,lb,decls,rb))
	| Ast0.TypeName(typename,name) ->
	    let (typename_n,typename) = string_mcode typename in
	    let (name_n,name) = ident name in
	    (bind typename_n name_n, Ast0.TypeName(typename,name))
	| Ast0.TypeOfExpr(tf,lp,exp,rp) ->
	    let (tf_n,tf) = string_mcode tf in
	    let (lp_n,lp) = string_mcode lp in
	    let (exp_n,exp) = expression exp in
            let (rp_n,rp) = string_mcode rp in
	    (multibind [tf_n;lp_n;exp_n;rp_n], Ast0.TypeOfExpr(tf,lp,exp,rp))
	| Ast0.TypeOfType(tf,lp,ty,rp) ->
	    let (tf_n,tf) = string_mcode tf in
	    let (lp_n,lp) = string_mcode lp in
	    let (ty_n,ty) = typeC ty in
            let (rp_n,rp) = string_mcode rp in
	    (multibind [tf_n;lp_n;ty_n;rp_n], Ast0.TypeOfType(tf,lp,ty,rp))
	| Ast0.NamedType(name) ->
	    let (name_n,name) = string_mcode name in
	    (name_n,Ast0.NamedType(name))
        | Ast0.QualifiedType(ty,coloncolon,name) ->
            let (ty_n,ty) = get_option typeC ty in
            let (coloncolon_n,coloncolon) = string_mcode coloncolon in
            let (name_n,name) = ident name in
            (multibind [ty_n;coloncolon_n;coloncolon_n;name_n], Ast0.QualifiedType(ty,coloncolon,name))
	| Ast0.AutoType(auto) ->
	    let (lauto, auto) = string_mcode auto in
	    (lauto, Ast0.AutoType(auto))
	| Ast0.MetaType(name,cstr,pure) ->
	    let (name_n,name) = meta_mcode name in
	    (name_n,Ast0.MetaType(name,cstr,pure))
	| Ast0.DisjType(starter,types,mids,ender) ->
	    do_disj starter types mids ender typeC
	      (fun starter types mids ender ->
		Ast0.DisjType(starter,types,mids,ender))
	| Ast0.ConjType(starter,types,mids,ender) ->
	    do_disj starter types mids ender typeC
	      (fun starter types mids ender ->
		Ast0.ConjType(starter,types,mids,ender))
	| Ast0.OptType(ty) ->
	    let (ty_n,ty) = typeC ty in (ty_n, Ast0.OptType(ty))
	| Ast0.AsType(ty,asty) ->
	    let (ty_n,ty) = typeC ty in
	    let (asty_n,asty) = typeC asty in
	    (bind ty_n asty_n, Ast0.AsType(ty,asty))
	| Ast0.TemplateType(tn,lp,args,rp) ->
	    let (tn_n,tn) = typeC tn in
	    let (lp_n,lp) = string_mcode lp in
	    let (args_n,args) = expression_dots args in
	    let (rp_n,rp) = string_mcode rp in
	    (multibind [tn_n;lp_n;args_n;rp_n], Ast0.TemplateType(tn,lp,args,rp))) in
    tyfn all_functions k t

  (* returns ((bind value,original value),id) since id may have been updated*)
  and array_type (ty,(id : Ast0.ident option),lb,size,rb) =
    let (ty_n,ty) = typeC ty in
    let (idl,idu) = (match id with
      | Some a -> let (b,c) = ident a in ([b],Some c)
      | None -> ([],None)) in
    let (lb_n,lb) = string_mcode lb in
    let (size_n,size) = get_option expression size in
    let (rb_n,rb) = string_mcode rb in
    ((multibind ([ty_n] @ idl @ [lb_n;size_n;rb_n]),
      Ast0.Array(ty,lb,size,rb)), idu)

  and parentype_type (lp,ty,(id : Ast0.ident option),rp) =
    let function_pointer ty1 array_decs =
      match Ast0.unwrap ty1 with
        Ast0.Pointer(ty2,star) ->
          (match Ast0.unwrap ty2 with
            Ast0.FunctionType(ty3,lp3,params,rp3) ->
              let (ty_n,typ) = typeC ty3 in
              let (lp_n,lp) = string_mcode lp in
              let (star_n,star) = unary_mcode star in
              let (idl,idu) =
                match id with
                  Some a -> let (b,c) = ident a in ([b],Some c)
                | None -> ([],None) in
              let (array_n, array_t) =
                match array_decs with
                  Some(lb1,size1,rb1) ->
                    let (lb1_n,lb1) = string_mcode lb1 in
                    let (size_n,size1) = get_option expression size1 in
                    let (rb1_n,rb1) = string_mcode rb1 in
                    ([lb1_n;size_n;rb1_n],
                     Some(lb1,size1,rb1))
                | None -> ([], None) in
              let (rp_n,rp) = string_mcode rp in
              let (lp3_n,lp3) = string_mcode lp3 in
              let (params_n,params) = parameter_dots params in
              let (rp3_n,rp3) = string_mcode rp3 in
              let bind_val =
                multibind ([ty_n;lp_n;star_n]
                @ idl @ array_n @ [rp_n;lp3_n;params_n;rp3_n]) in
              let inner_type =
                let inner_type1 =
                  Ast0.rewrap ty2
                    (Ast0.Pointer
                       (Ast0.rewrap ty3
                          (Ast0.FunctionType
                             (typ,lp3,params,rp3)),star)) in
                match array_t with
                    Some(lb1,size1,rb1) ->
                      Ast0.rewrap ty1
                        (Ast0.Array(inner_type1,lb1,size1,rb1))
                  | None -> inner_type1 in
              ((bind_val, Ast0.ParenType (lp,inner_type,rp)), idu)
        | _ -> failwith "ParenType Visitor_ast0")
      | _ -> failwith "ParenType Visitor_ast0" in
    match Ast0.unwrap ty with
      Ast0.Array(ty1,lb1,size,rb1) ->
        function_pointer ty1 (Some(lb1,size,rb1))
    | Ast0.Pointer(ty1,star) ->
        function_pointer ty None
    | _ -> failwith "ParenType Visitor_ast0"

  and functiontype_type (ty,(id : Ast0.ident option),lp,params,rp) =
    let (ty_n,ty) = typeC ty in
    let (idl,idu) = (match id with
      | Some a -> let (b,c) = ident a in ([b],Some c)
      | None -> ([],None)) in
    let (lp_n,lp) = string_mcode lp in
    let (params_n,params) = parameter_dots params in
    let (rp_n,rp) = string_mcode rp in
    ((multibind ([ty_n] @ idl @ [lp_n; params_n; rp_n]),
     Ast0.FunctionType(ty,lp,params,rp)), idu)

  and named_type ty id =
    match Ast0.unwrap ty with
      Ast0.Array(rty,lb,size,rb) ->
	let (tyres, idn) = array_type (rty,Some id,lb,size,rb) in
        let idn = match idn with Some i -> i | None -> failwith "Impossible" in
	(rewrap ty tyres, idn)
    | Ast0.ParenType(lp,rty,rp) ->
	let (tyres, idn) = parentype_type (lp,rty,Some id,rp) in
	let idn = match idn with Some i -> i | None -> failwith "Impossible" in
        (rewrap ty tyres, idn)
    | Ast0.FunctionType(rty,lp,params,rp) ->
	let (tyres, idn) = functiontype_type (rty,Some id,lp,params,rp) in
	let idn = match idn with Some i -> i | None -> failwith "Impossible" in
        (rewrap ty tyres, idn)
    | _ -> let (ty_n,ty) = typeC ty in
           let (id_n,id) = ident id in
           (((multibind [ty_n;id_n]), ty), id)

  (* returns ((bind value,original value),id) since id may have been updated*)
  and array_type_typedef (ty,id,lb,size,rb) =
    let (ty_n,ty) = typeC ty in
    let (idl,idu) = (match id with
      | Some a -> let (b,c) = typeC a in ([b],Some c)
      | None -> ([],None)) in
    let (lb_n,lb) = string_mcode lb in
    let (size_n,size) = get_option expression size in
    let (rb_n,rb) = string_mcode rb in
    ((multibind ([ty_n] @ idl @ [lb_n;size_n;rb_n]),
     Ast0.Array(ty,lb,size,rb)), idu)

  (* returns ((bind value,original value),id) since id may have been updated*)
  and parentype_typedef (lp,ty,id,rp) =
    let function_pointer ty1 array_decs =
      match Ast0.unwrap ty1 with
        Ast0.Pointer(ty2,star) ->
          (match Ast0.unwrap ty2 with
            Ast0.FunctionType(ty3,lp3,params,rp3) ->
              let (ty_n,typ) = typeC ty3 in
              let (lp_n,lp) = string_mcode lp in
              let (star_n,star) = unary_mcode star in
              let (idl,idu) =
                match id with
                  Some a -> let (b,c) = typeC a in ([b],Some c)
                | None -> ([],None) in
              let (array_n, array_t) =
                match array_decs with
                  Some(lb1,size1,rb1) ->
                    let (lb1_n,lb1) = string_mcode lb1 in
                    let (size_n,size1) = get_option expression size1 in
                    let (rb1_n,rb1) = string_mcode rb1 in
                    ([lb1_n;size_n;rb1_n],
                     Some(lb1,size1,rb1))
                | None -> ([], None) in
              let (rp_n,rp) = string_mcode rp in
              let (lp3_n,lp3) = string_mcode lp3 in
              let (params_n,params) = parameter_dots params in
              let (rp3_n,rp3) = string_mcode rp3 in
              let bind_val =
                multibind ([ty_n;lp_n;star_n]
                @ idl @ array_n @ [rp_n;lp3_n;params_n;rp3_n]) in
              let inner_type =
                let inner_type1 =
                Ast0.rewrap ty2
                  (Ast0.Pointer
                     (Ast0.rewrap ty3
                        (Ast0.FunctionType
                           (typ,lp3,params,rp3)),star)) in
                match array_t with
                    Some(lb1,size1,rb1) ->
                      Ast0.rewrap ty1
                        (Ast0.Array(inner_type1,lb1,size1,rb1))
                  | None -> inner_type1 in
              ((bind_val, Ast0.ParenType (lp,inner_type,rp)), idu)
        | _ -> failwith "ParenType Visitor_ast0")
      | _ -> failwith "ParenType Visitor_ast0" in
    match Ast0.unwrap ty with
      Ast0.Array(ty1,lb1,size,rb1) ->
        function_pointer ty1 (Some(lb1,size,rb1))
    | Ast0.Pointer(ty1,star) ->
        function_pointer ty None
    | _ -> failwith "ParenType Visitor_ast0"

  and functiontype_typedef (ty,id,lp,params,rp) =
    let (ty_n,ty) = typeC ty in
    let (idl,idu) = (match id with
      | Some a -> let (b,c) = typeC a in ([b],Some c)
      | None -> ([],None)) in
    let (lp_n,lp) = string_mcode lp in
    let (params_n,params) = parameter_dots params in
    let (rp_n,rp) = string_mcode rp in
    ((multibind ([ty_n] @ idl @ [lp_n; params_n; rp_n]),
     Ast0.FunctionType(ty,lp,params,rp)), idu)

  and named_type_typedef ty id =
    match Ast0.unwrap ty with
      Ast0.Array(rty,lb,size,rb) ->
	let (tyres, idn) = array_type_typedef (rty,Some id,lb,size,rb) in
        let idn = match idn with Some i -> i | None -> failwith "Impossible" in
	(rewrap ty tyres, idn)
    | Ast0.ParenType(lp,rty,rp) ->
	let (tyres, idn) = parentype_typedef (lp,rty,Some id,rp) in
	let idn = match idn with Some i -> i | None -> failwith "Impossible" in
	(rewrap ty tyres, idn)
    | Ast0.FunctionType(rty,lp,params,rp) ->
	let (tyres, idn) = functiontype_typedef (rty,Some id,lp,params,rp) in
	let idn = match idn with Some i -> i | None -> failwith "Impossible" in
	(rewrap ty tyres, idn)
    | _ -> let (ty_n,ty) = typeC ty in
           let (id_n,id) = typeC id in
             ((bind ty_n id_n, ty), id)

  and alignas (Ast0.Align(align,lpar,expr,rpar)) =
    let (lalign_n,lalign) = string_mcode align in
    let (llp_n,llp) = string_mcode lpar in
    let (lexpr_n,lexpr) = expression expr in
    let (lrp_n,lrp) = string_mcode rpar in
    (multibind [lalign_n; llp_n; lexpr_n; lrp_n],
    Ast0.Align(lalign,llp,lexpr,lrp))

  and declaration d =
    let k d =
      rewrap d
	(match Ast0.unwrap d with
	  Ast0.MetaDecl(name,constraints,pure) ->
	    let (n,name) = meta_mcode name in
	    (n,Ast0.MetaDecl(name,constraints,pure))
	| Ast0.Init(al,stg,ty,id,endattr,eq,ini,sem) ->
	    let (al_n,al) = get_option alignas al in
	    let (stg_n,stg) = get_option storage_mcode stg in
	    let ((ty_ma_id_n,ty),id) = named_type ty id in
	    let (endattr_n,endattr) = map_split_bind attribute endattr in
	    let (eq_n,eq) = string_mcode eq in
	    let (ini_n,ini) = initialiser ini in
	    let (sem_n,sem) = get_option string_mcode sem in
	    (multibind [al_n;stg_n;ty_ma_id_n;endattr_n;eq_n;ini_n;sem_n],
	     Ast0.Init(al,stg,ty,id,endattr,eq,ini,sem))
	| Ast0.UnInit(al,stg,ty,id,endattr,sem) ->
	    let (al_n,al) = get_option alignas al in
	    let (stg_n,stg) = get_option storage_mcode stg in
	    let ((ty_ma_id_n,ty),id) = named_type ty id in
	    let (endattr_n,endattr) = map_split_bind attribute endattr in
	    let (sem_n,sem) = string_mcode sem in
            (multibind [al_n;stg_n;ty_ma_id_n;endattr_n;sem_n],
	     Ast0.UnInit(al,stg,ty,id,endattr,sem))
	| Ast0.FunProto(fi,name,lp1,params,va,rp1,sem) ->
	    let (fi_n,fi) = map_split_bind fninfo fi in
	    let (name_n,name) = ident name in
	    let (lp1_n,lp1) = string_mcode lp1 in
	    let (params_n,params) = parameter_dots params in
	    let (va_n,va) = begin match va with
	      | None -> (option_default, None)
	      | Some (comma, ellipsis) ->
	        let (comma_n,comma) = string_mcode comma in
	        let (ellipsis_n,ellipsis) = string_mcode ellipsis in
	        (multibind [comma_n; ellipsis_n],Some(comma,ellipsis)) end in
	    let (rp1_n,rp1) = string_mcode rp1 in
	    let (sem_n,sem) = string_mcode sem in
            (multibind [fi_n;name_n;lp1_n;params_n;va_n;rp1_n;sem_n],
	     Ast0.FunProto(fi,name,lp1,params,va,rp1,sem))
	| Ast0.MacroDecl(stg,preattr,name,lp,args,rp,attr,sem) ->
	    let (stg_n,stg) = get_option storage_mcode stg in
	    let (preattr_n,preattr) = map_split_bind attribute preattr in
	    let (name_n,name) = ident name in
	    let (lp_n,lp) = string_mcode lp in
	    let (args_n,args) = expression_dots args in
	    let (rp_n,rp) = string_mcode rp in
	    let (attr_n,attr) = map_split_bind attribute attr in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [stg_n;preattr_n;name_n;lp_n;args_n;rp_n;attr_n;sem_n],
	     Ast0.MacroDecl(stg,preattr,name,lp,args,rp,attr,sem))
	| Ast0.MacroDeclInit(stg,preattr,name,lp,args,rp,attr,eq,ini,sem) ->
	    let (stg_n,stg) = get_option storage_mcode stg in
	    let (preattr_n,preattr) = map_split_bind attribute preattr in
	    let (name_n,name) = ident name in
	    let (lp_n,lp) = string_mcode lp in
	    let (args_n,args) = expression_dots args in
	    let (rp_n,rp) = string_mcode rp in
	    let (attr_n,attr) = map_split_bind attribute attr in
	    let (eq_n,eq) = string_mcode eq in
	    let (ini_n,ini) = initialiser ini in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [stg_n;preattr_n;name_n;lp_n;args_n;rp_n;attr_n;eq_n;ini_n;sem_n],
	     Ast0.MacroDeclInit(stg,preattr,name,lp,args,rp,attr,eq,ini,sem))
	| Ast0.TyDecl(ty,sem) ->
	    let (ty_n,ty) = typeC ty in
	    let (sem_n,sem) = string_mcode sem in
            (multibind [ty_n; sem_n], Ast0.TyDecl(ty,sem))
	| Ast0.Typedef(stg,ty,id,sem) ->
	    let (stg_n,stg) = string_mcode stg in
	    let ((ty_id_n,ty),id) = named_type_typedef ty id in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [stg_n;ty_id_n;sem_n], Ast0.Typedef(stg,ty,id,sem))
	| Ast0.DisjDecl(starter,decls,mids,ender) ->
	    do_disj starter decls mids ender declaration
	      (fun starter decls mids ender ->
		Ast0.DisjDecl(starter,decls,mids,ender))
	| Ast0.ConjDecl(starter,decls,mids,ender) ->
	    do_disj starter decls mids ender declaration
	      (fun starter decls mids ender ->
		Ast0.ConjDecl(starter,decls,mids,ender))
	| Ast0.OptDecl(decl) ->
	    let (n,decl) = declaration decl in (n,Ast0.OptDecl(decl))
	| Ast0.AsDecl(decl,asdecl) ->
	    let (decl_n,decl) = declaration decl in
	    let (asdecl_n,asdecl) = declaration asdecl in
	    (bind decl_n asdecl_n, Ast0.AsDecl(decl,asdecl))) in
    declfn all_functions k d

  and field d =
    let k d =
      rewrap d
	(match Ast0.unwrap d with
	  Ast0.MetaField(name,constraints,pure) ->
	    let (n,name) = meta_mcode name in
	    (n,Ast0.MetaField(name,constraints,pure))
	| Ast0.MetaFieldList(name,lenname,constraints,pure) ->
	    let (n,name) = meta_mcode name in
	    (n,Ast0.MetaFieldList(name,lenname,constraints,pure))
	| Ast0.Field(ty,id,bf,endattr,sem) ->
	    let ((ty_id_n,ty),id) =
	      match id with
		None -> (typeC ty, None)
	      | Some id ->
                  let (ty, id) = named_type ty id in
		  (ty, Some id) in
	    let bitfield (c, e) =
	      let (c_n, c) = string_mcode c in
	      let (e_n, e) = expression e in
	      ([c_n; e_n], Some (c, e)) in
	    let (bf_n,bf) = Common.default ([], None) bitfield bf in
	    let (endattr_n,endattr) = map_split_bind attribute endattr in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind ([ty_id_n] @ bf_n @ [endattr_n;sem_n]),
	     Ast0.Field(ty,id,bf,endattr,sem))
	| Ast0.MacroDeclField(name,lp,args,rp,attr,sem) ->
	    let (name_n,name) = ident name in
	    let (lp_n,lp) = string_mcode lp in
	    let (args_n,args) = expression_dots args in
	    let (rp_n,rp) = string_mcode rp in
	    let (attr_n,attr) = map_split_bind attribute attr in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [name_n;lp_n;args_n;rp_n;attr_n;sem_n],
	     Ast0.MacroDeclField(name,lp,args,rp,attr,sem))
	| Ast0.CppField(di) ->
	    let (di_n,di) = directive di in
	    (di_n, Ast0.CppField(di))
	| Ast0.DisjField(starter,decls,mids,ender) ->
	    do_disj starter decls mids ender field
	      (fun starter decls mids ender ->
		Ast0.DisjField(starter,decls,mids,ender))
	| Ast0.ConjField(starter,decls,mids,ender) ->
	    do_disj starter decls mids ender field
	      (fun starter decls mids ender ->
		Ast0.ConjField(starter,decls,mids,ender))
	| Ast0.AccSpec(decl,dd) ->
	    let (decl_n,decl) = string_mcode decl in
	    let (dd_n,dd) = string_mcode dd in
	    (multibind [dd_n;decl_n], Ast0.AccSpec(decl,dd))
	| Ast0.Fdots(dots,whencode) ->
	    let (dots_n,dots) = string_mcode dots in
	    let (whencode_n, whencode) =
	      match whencode with
		Some (a,b,c) ->
                  let (_,a2) = string_mcode a in
                  let (_,b2) = string_mcode b in
                  let (c1,c2) = field c in
		  (c1, Some (a2,b2,c2))
              | None -> (option_default, None) in
	    (bind dots_n whencode_n, Ast0.Fdots(dots,whencode))
	| Ast0.OptField(decl) ->
	    let (n,decl) = field decl in (n,Ast0.OptField(decl))) in
    fieldfn all_functions k d

  and enum_base base =
    let (tdd, typ) = base in
    let (tdd_n, tdd) = string_mcode tdd in
    let (typ_n, typ) = typeC typ in
    ((bind tdd_n typ_n), (tdd, typ))

  and enum_decl d =
    let k d =
      rewrap d
	(match Ast0.unwrap d with
	  Ast0.Enum(name,enum_val) ->
	    let (name_n,name) = ident name in
            (match enum_val with
              None -> (name_n,Ast0.Enum(name,None))
            | Some(eq,eval) ->
                let (eq_n,eq) = string_mcode eq in
                let (eval_n,eval) = expression eval in
                (multibind [name_n; eq_n; eval_n],
                 Ast0.Enum(name,Some(eq,eval))))
	| Ast0.EnumComma(cm) ->
	    let (cm_n,cm) = string_mcode cm in
	    (cm_n,Ast0.EnumComma(cm))
	| Ast0.EnumDots(dots,whencode) ->
	    let (dots_n,dots) = string_mcode dots in
	    let (whencode_n, whencode) =
	      match whencode with
		Some (a,b,c) ->
                  let (_,a2) = string_mcode a in
                  let (_,b2) = string_mcode b in
                  let (c1,c2) = enum_decl c in
		  (c1, Some (a2,b2,c2))
              | None -> (option_default, None) in
	    (bind dots_n whencode_n, Ast0.EnumDots(dots,whencode))) in
    enumdeclfn all_functions k d


  and initialiser i =
    let k i =
      rewrap i
	(match Ast0.unwrap i with
	  Ast0.MetaInit(name,constraints,pure) ->
	    let (name_n,name) = meta_mcode name in
	    (name_n,Ast0.MetaInit(name,constraints,pure))
	| Ast0.MetaInitList(name,lenname,constraints,pure) ->
	    let (name_n,name) = meta_mcode name in
	    (name_n,Ast0.MetaInitList(name,lenname,constraints,pure))
	| Ast0.InitExpr(exp) ->
	    let (exp_n,exp) = expression exp in
	    (exp_n,Ast0.InitExpr(exp))
	| Ast0.InitList(lb,initlist,rb,ordered) ->
	    let (lb_n,lb) = string_mcode lb in
	    let (initlist_n,initlist) = initialiser_dots initlist in
	    let (rb_n,rb) = string_mcode rb in
	    (multibind [lb_n;initlist_n;rb_n],
	     Ast0.InitList(lb,initlist,rb,ordered))
	| Ast0.InitGccExt(designators,eq,ini) ->
	    let (dn,designators) = map_split_bind designator designators in
	    let (eq_n,eq) = string_mcode eq in
	    let (ini_n,ini) = initialiser ini in
	    (multibind [dn;eq_n;ini_n], Ast0.InitGccExt(designators,eq,ini))
	| Ast0.InitGccName(name,eq,ini) ->
	    let (name_n,name) = ident name in
	    let (eq_n,eq) = string_mcode eq in
	    let (ini_n,ini) = initialiser ini in
	    (multibind [name_n;eq_n;ini_n], Ast0.InitGccName(name,eq,ini))
	| Ast0.IComma(cm) ->
	    let (n,cm) = string_mcode cm in (n,Ast0.IComma(cm))
	| Ast0.Idots(d,whencode) ->
	    let (d_n,d) = string_mcode d in
	    let (whencode_n, whencode) =
	      match whencode with
		Some (a,b,c) ->
		  let (_,a2) = string_mcode a in
		  let (_,b2) = string_mcode b in
		  let (c1,c2) = initialiser c in
		  (c1, Some (a2,b2,c2))
	      | None -> (option_default, None) in
	    (bind d_n whencode_n, Ast0.Idots(d,whencode))
	| Ast0.OptIni(i) ->
	    let (n,i) = initialiser i in (n,Ast0.OptIni(i))
	| Ast0.AsInit(ini,asini) ->
	    let (ini_n,ini) = initialiser ini in
	    let (asini_n,asini) = initialiser asini in
	    (bind ini_n asini_n, Ast0.AsInit(ini,asini))) in
    initfn all_functions k i

  and designator = function
      Ast0.DesignatorField(dot,id) ->
	let (dot_n,dot) = string_mcode dot in
	let (id_n,id) = ident id in
	(bind dot_n id_n, Ast0.DesignatorField(dot,id))
    | Ast0.DesignatorIndex(lb,exp,rb) ->
	let (lb_n,lb) = string_mcode lb in
	let (exp_n,exp) = expression exp in
	let (rb_n,rb) = string_mcode rb in
	(multibind [lb_n;exp_n;rb_n], Ast0.DesignatorIndex(lb,exp,rb))
    | Ast0.DesignatorRange(lb,min,dots,max,rb) ->
	let (lb_n,lb) = string_mcode lb in
	let (min_n,min) = expression min in
	let (dots_n,dots) = string_mcode dots in
	let (max_n,max) = expression max in
	let (rb_n,rb) = string_mcode rb in
	(multibind [lb_n;min_n;dots_n;max_n;rb_n],
	 Ast0.DesignatorRange(lb,min,dots,max,rb))

  and parameterTypeDef p =
    let k p =
      rewrap p
	(match Ast0.unwrap p with
	  Ast0.Param(ty,Some id,attrs) ->
            let ((ty_mid_id_n,ty),id) = named_type ty id in
	    let (attr_n,attr) = map_split_bind attribute attrs in
	    (bind ty_mid_id_n attr_n, Ast0.Param(ty,Some id,attr))
	| Ast0.Param(ty,None,attrs) ->
	    let (ty_n,ty) = typeC ty in
	    let (attr_n,attr) = map_split_bind attribute attrs in
            (bind ty_n attr_n, Ast0.Param(ty,None,attr))
	| Ast0.MetaParam(name,constraints,pure) ->
	    let (n,name) = meta_mcode name in
	    (n,Ast0.MetaParam(name,constraints,pure))
	| Ast0.MetaParamList(name,lenname,constraints,pure) ->
	    let (n,name) = meta_mcode name in
	    (n,Ast0.MetaParamList(name,lenname,constraints,pure))
	| Ast0.AsParam(p,asexp) ->
	    let (p_n,p) = parameterTypeDef p in
	    let (asexp_n,asexp) = expression asexp in
	    (bind p_n asexp_n, Ast0.AsParam(p,asexp))
	| Ast0.PComma(cm) ->
	    let (n,cm) = string_mcode cm in (n,Ast0.PComma(cm))
	| Ast0.Pdots(dots) ->
	    let (n,dots) = string_mcode dots in (n,Ast0.Pdots(dots))
	| Ast0.OptParam(param) ->
	    let (n,param) = parameterTypeDef param in
	    (n,Ast0.OptParam(param))) in
    paramfn all_functions k p

  and templateParameterTypeDef p =
    let k p =
      rewrap p
	(match Ast0.unwrap p with
          Ast0.TypenameOrClassParam(tyorcl,id,Some (eq,ty)) ->
	    let (tyorcl_n,tyorcl) = string_mcode tyorcl in
	    let (id_n,id) = ident id in
	    let (eq_n,eq) = string_mcode eq in
	    let (ty_n,ty) = typeC ty in
	    (bind tyorcl_n ty_n, Ast0.TypenameOrClassParam(tyorcl,id,Some (eq,ty)))
        | Ast0.TypenameOrClassParam(tyorcl,id,None) ->
	    let (tyorcl_n,tyorcl) = string_mcode tyorcl in
	    let (id_n,id) = ident id in
	    (bind tyorcl_n id_n, Ast0.TypenameOrClassParam(tyorcl,id,None))
        | Ast0.VarNameParam(ty,id,Some (eq,ini)) ->
	    let (ty_n,ty) = typeC ty in
	    let (id_n,id) = ident id in
	    let (eq_n,eq) = string_mcode eq in
	    let (ini_n,ini) = initialiser ini in
	    (bind ty_n ini_n, Ast0.VarNameParam(ty,id,Some (eq,ini)))
        | Ast0.VarNameParam(ty,id,None) ->
	    let (ty_n,ty) = typeC ty in
	    let (id_n,id) = ident id in
	    (bind ty_n id_n, Ast0.VarNameParam(ty,id,None))
        | Ast0.TPComma(comma) ->
	    let (comma_n,comma) = string_mcode comma in
	    (comma_n, Ast0.TPComma(comma))
        | Ast0.TPDots(dots) ->
	    let (dots_n,dots) = string_mcode dots in
	    (dots_n, Ast0.TPComma(dots))
        ) in
    template_paramfn all_functions k p


  (* not done for combiner, because the statement is assumed to be already
     represented elsewhere in the code *)
  (* NOTE: This is not called for combiner_rebuilder.  This is ok for its
     only current use. *)
  and process_bef_aft s =
    Ast0.set_dots_bef_aft s
      (match Ast0.get_dots_bef_aft s with
	Ast0.NoDots -> Ast0.NoDots
      | Ast0.DroppingBetweenDots(stm) ->
	  let (_,stm) = statement stm in Ast0.DroppingBetweenDots(stm)
      | Ast0.AddingBetweenDots(stm) ->
	  let (_,stm) = statement stm in Ast0.AddingBetweenDots(stm))

  and statement s =
    (if mode = COMBINER then let _ = process_bef_aft s in ());
    let k s =
      rewrap s
	(match Ast0.unwrap s with
	  Ast0.FunDecl(bef,fi,name,lp,params,va,rp,attrs,lbrace,body,rbrace,aft) ->
	    let (fi_n,fi) = map_split_bind fninfo fi in
	    let (name_n,name) = ident name in
	    let (lp_n,lp) = string_mcode lp in
	    let (params_n,params) = parameter_dots params in
            let (va_n,va) = match va with
              | None -> (option_default, None)
              | Some (comma,ellipsis) ->
                let (comma_n, comma) = string_mcode comma in
                let (ellipsis_n, ellipsis) = string_mcode ellipsis in
                (multibind [comma_n;ellipsis_n],Some(comma,ellipsis)) in
	    let (rp_n,rp) = string_mcode rp in
	    let (attr_n,attr) = map_split_bind attribute attrs in
	    let (lbrace_n,lbrace) = string_mcode lbrace in
	    let (body_n,body) = statement_dots body in
	    let (rbrace_n,rbrace) = string_mcode rbrace in
	    (multibind
               [fi_n;name_n;lp_n;params_n;va_n;rp_n;attr_n;lbrace_n;body_n;rbrace_n],
	     Ast0.FunDecl(bef,fi,name,lp,params,va,rp,attr,lbrace,body,rbrace,aft))
        | Ast0.TemplateDefinition(tmpkw,lab,params,rab,stmt) ->
	    let (tmpkw_n,tmpkw) = string_mcode tmpkw in
	    let (lab_n,lab) = string_mcode lab in
	    let (params_n,params) = template_parameter_dots params in
	    let (rab_n,rab) = string_mcode rab in
	    let (stmt_n,stmt) = statement stmt in
	    (multibind [tmpkw_n;lab_n;params_n;rab_n;stmt_n],
	     Ast0.TemplateDefinition(tmpkw,lab,params,rab,stmt))
	| Ast0.Decl(bef,decl) ->
	    let (decl_n,decl) = declaration decl in
	    (decl_n,Ast0.Decl(bef,decl))
	| Ast0.Seq(lbrace,body,rbrace) ->
	    let (lbrace_n,lbrace) = string_mcode lbrace in
	    let (body_n,body) = statement_dots body in
	    let (rbrace_n,rbrace) = string_mcode rbrace in
	    (multibind [lbrace_n;body_n;rbrace_n],
	     Ast0.Seq(lbrace,body,rbrace))
	| Ast0.ExprStatement(exp,sem) ->
	    let (exp_n,exp) = get_option expression exp in
	    let (sem_n,sem) = string_mcode sem in
	    (bind exp_n sem_n, Ast0.ExprStatement(exp,sem))
	| Ast0.IfThen(iff,lp,exp,rp,branch1,aft) ->
	    let (iff_n,iff) = string_mcode iff in
	    let (lp_n,lp) = string_mcode lp in
	    let (exp_n,exp) = expression exp in
	    let (rp_n,rp) = string_mcode rp in
	    let (branch1_n,branch1) = statement branch1 in
	    (multibind [iff_n;lp_n;exp_n;rp_n;branch1_n],
	     Ast0.IfThen(iff,lp,exp,rp,branch1,aft))
	| Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) ->
	    let (iff_n,iff) = string_mcode iff in
	    let (lp_n,lp) = string_mcode lp in
	    let (exp_n,exp) = expression exp in
	    let (rp_n,rp) = string_mcode rp in
	    let (branch1_n,branch1) = statement branch1 in
	    let (els_n,els) = string_mcode els in
	    let (branch2_n,branch2) = statement branch2 in
	    (multibind [iff_n;lp_n;exp_n;rp_n;branch1_n;els_n;branch2_n],
	     Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft))
	| Ast0.While(whl,lp,cond,rp,body,aft) ->
	    let (whl_n,whl) = string_mcode whl in
	    let (lp_n,lp) = string_mcode lp in
	    let (cond_n,cond) = whileinfo cond in
	    let (rp_n,rp) = string_mcode rp in
	    let (body_n,body) = statement body in
	    (multibind [whl_n;lp_n;cond_n;rp_n;body_n],
	     Ast0.While(whl,lp,cond,rp,body,aft))
	| Ast0.ScopedGuard(sg,lp,exps,rp,body,aft) ->
	    let (sg_n,sg) = string_mcode sg in
	    let (lp_n,lp) = string_mcode lp in
	    let (exps_n,exps) = expression_dots exps in
	    let (rp_n,rp) = string_mcode rp in
	    let (body_n,body) = statement body in
	    (multibind [sg_n;lp_n;exps_n;rp_n;body_n],
	     Ast0.ScopedGuard(sg,lp,exps,rp,body,aft))
	| Ast0.Do(d,body,whl,lp,exp,rp,sem) ->
	    let (d_n,d) = string_mcode d in
	    let (body_n,body) = statement body in
	    let (whl_n,whl) = string_mcode whl in
	    let (lp_n,lp) = string_mcode lp in
	    let (exp_n,exp) = expression exp in
	    let (rp_n,rp) =  string_mcode rp in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [d_n;body_n;whl_n;lp_n;exp_n;rp_n;sem_n],
	     Ast0.Do(d,body,whl,lp,exp,rp,sem))
	| Ast0.For(fr,lp,first,rp,body,aft) ->
	    let (fr_n,fr) = string_mcode fr in
	    let (lp_n,lp) = string_mcode lp in
	    let (first_n,first) = forinfo first in
	    let (rp_n,rp) = string_mcode rp in
	    let (body_n,body) = statement body in
	    (multibind [fr_n;lp_n;first_n;rp_n;body_n],
	     Ast0.For(fr,lp,first,rp,body,aft))
	| Ast0.Iterator(nm,lp,args,rp,body,aft) ->
	    let (nm_n,nm) = ident nm in
	    let (lp_n,lp) = string_mcode lp in
	    let (args_n,args) = expression_dots args in
	    let (rp_n,rp) = string_mcode rp in
	    let (body_n,body) = statement body in
	    (multibind [nm_n;lp_n;args_n;rp_n;body_n],
	     Ast0.Iterator(nm,lp,args,rp,body,aft))
	| Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) ->
	    let (switch_n,switch) = string_mcode switch in
	    let (lp_n,lp) = string_mcode lp in
	    let (exp_n,exp) = expression exp in
	    let (rp_n,rp) = string_mcode rp in
	    let (lb_n,lb) = string_mcode lb in
	    let (decls_n,decls) = statement_dots decls in
	    let (cases_n,cases) = case_line_dots cases in
	    let (rb_n,rb) = string_mcode rb in
	    (multibind [switch_n;lp_n;exp_n;rp_n;lb_n;decls_n;cases_n;rb_n],
      	     Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb))
	| Ast0.Break(br,sem) ->
	    let (br_n,br) = string_mcode br in
	    let (sem_n,sem) = string_mcode sem in
	    (bind br_n sem_n, Ast0.Break(br,sem))
	| Ast0.Continue(cont,sem) ->
	    let (cont_n,cont) = string_mcode cont in
	    let (sem_n,sem) = string_mcode sem in
	    (bind cont_n sem_n, Ast0.Continue(cont,sem))
	| Ast0.Label(l,dd) ->
	    let (l_n,l) = ident l in
	    let (dd_n,dd) = string_mcode dd in
	    (bind l_n dd_n, Ast0.Label(l,dd))
	| Ast0.Goto(goto,l,sem) ->
	    let (goto_n,goto) = string_mcode goto in
	    let (l_n,l) = ident l in
	    let (sem_n,sem) = string_mcode sem in
	    (bind goto_n (bind l_n sem_n), Ast0.Goto(goto,l,sem))
	| Ast0.Return(ret,sem) ->
	    let (ret_n,ret) = string_mcode ret in
	    let (sem_n,sem) = string_mcode sem in
	    (bind ret_n sem_n, Ast0.Return(ret,sem))
	| Ast0.ReturnExpr(ret,exp,sem) ->
	    let (ret_n,ret) = string_mcode ret in
	    let (exp_n,exp) = expression exp in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [ret_n;exp_n;sem_n], Ast0.ReturnExpr(ret,exp,sem))
	| Ast0.Exec(exec,lang,code,sem) ->
	    let (exec_n,exec) = string_mcode exec in
	    let (lang_n,lang) = string_mcode lang in
	    let (code_n,code) = exec_code_dots code in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [exec_n;lang_n;code_n;sem_n],
	     Ast0.Exec(exec,lang,code,sem))
	| Ast0.MetaStmt(name,constraints,pure) ->
	    let (name_n,name) = meta_mcode name in
	    (name_n,Ast0.MetaStmt(name,constraints,pure))
	| Ast0.MetaStmtList(name,lenname,constraints,pure) ->
	    let (name_n,name) = meta_mcode name in
	    (name_n,Ast0.MetaStmtList(name,lenname,constraints,pure))
	| Ast0.Disj(starter,statement_dots_list,mids,ender) ->
	    do_disj starter statement_dots_list mids ender statement_dots
	      (fun starter statement_dots_list mids ender ->
		Ast0.Disj(starter,statement_dots_list,mids,ender))
	| Ast0.Conj(starter,statement_dots_list,mids,ender) ->
	    do_disj starter statement_dots_list mids ender statement_dots
	      (fun starter statement_dots_list mids ender ->
		Ast0.Conj(starter,statement_dots_list,mids,ender))
	| Ast0.Nest(starter,stmt_dots,ender,whn,multi) ->
	    let (starter_n,starter) = string_mcode starter in
	    let (stmt_dots_n,stmt_dots) = statement_dots stmt_dots in
	    let (whn_n,whn) =
	      map_split_bind (whencode statement_dots statement) whn in
	    let (ender_n,ender) = string_mcode ender in
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
	| Ast0.TopId(id) ->
	    let (id_n,id) = ident id in
	    (id_n,Ast0.TopId(id))
	| Ast0.TopInit(init) ->
	    let (init_n,init) = initialiser init in
	    (init_n,Ast0.TopInit(init))
	| Ast0.Dots(d,whn) ->
	    let (d_n,d) = string_mcode d in
	    let (whn_n,whn) =
	      map_split_bind (whencode statement_dots statement) whn in
	    (bind d_n whn_n, Ast0.Dots(d,whn))
	| Ast0.CppTop(di) ->
	    let (di_n,di) = directive di in
	    (di_n, Ast0.CppTop(di))
	| Ast0.Undef(def,id) ->
	    let (def_n,def) = string_mcode def in
	    let (id_n,id) = ident id in
	    (multibind [def_n;id_n],Ast0.Undef(def,id))
	| Ast0.Define(def,id,params,body) ->
	    let (def_n,def) = string_mcode def in
	    let (id_n,id) = ident id in
	    let (params_n,params) = define_parameters params in
	    let (body_n,body) =
	      match body with
		Ast0.DefineStms body ->
		  let (body_n,body) = statement_dots body in
		  (body_n, Ast0.DefineStms body)
	      | Ast0.DefineAttr attr ->
		  let (attr_n,attr) = attribute attr in
		  (attr_n, Ast0.DefineAttr attr) in
	    (multibind [def_n;id_n;params_n;body_n],
	     Ast0.Define(def,id,params,body))
	| Ast0.OptStm(re) ->
	    let (re_n,re) = statement re in (re_n,Ast0.OptStm(re))
	| Ast0.AsStmt(stm,asstm) ->
	    let (stm_n,stm) = statement stm in
	    let (asstm_n,asstm) = statement asstm in
	    (bind stm_n asstm_n, Ast0.AsStmt(stm,asstm))) in
    let (n,s) = stmtfn all_functions k s in
    (n,if mode = REBUILDER then process_bef_aft s else s)

  and whileinfo = function
      Ast0.WhileExp(e) ->
	let (n,e) = expression e in
	(n,Ast0.WhileExp(e))
    | Ast0.WhileDecl(bef,d) ->
	let (n,d) = declaration d in
	(n,Ast0.WhileDecl(bef,d))

  and forinfo fi =
    let k fi =
      rewrap fi
	(match Ast0.unwrap fi with
	  Ast0.ForExp(e1,sem1,e2,sem2,e3) ->
	    let (e1_n,e1) = get_option expression e1 in
	    let (sem1_n,sem1) = string_mcode sem1 in
	    let (e2_n,e2) = get_option expression e2 in
	    let (sem2_n,sem2) = string_mcode sem2 in
	    let (e3_n,e3) = get_option expression e3 in
	    (multibind [e1_n;sem1_n;e2_n;sem2_n;e3_n],
	     Ast0.ForExp(e1,sem1,e2,sem2,e3))
	| Ast0.ForDecl (bef,decl,e2,sem2,e3) ->
	    let (decl_n,decl) = declaration decl in
	    let (e2_n,e2) = get_option expression e2 in
	    let (sem2_n,sem2) = string_mcode sem2 in
	    let (e3_n,e3) = get_option expression e3 in
	    (multibind [decl_n;e2_n;sem2_n;e3_n],
	     Ast0.ForDecl (bef,decl,e2,sem2,e3))
	| Ast0.ForRange (bef,decl,ini) ->
	    let (decl_n,decl) = declaration decl in
	    let (ini_n,ini) = initialiser ini in
	    (multibind [decl_n;ini_n],Ast0.ForRange (bef,decl,ini))) in
    forinfofn all_functions k fi

  (* not parameterizable for now... *)
  and define_parameters p =
    let k p =
      rewrap p
	(match Ast0.unwrap p with
	  Ast0.NoParams -> (option_default,Ast0.NoParams)
	| Ast0.DParams(lp,params,rp) ->
	    let (lp_n,lp) = string_mcode lp in
	    let (params_n,params) = define_param_dots params in
	    let (rp_n,rp) = string_mcode rp in
	    (multibind [lp_n;params_n;rp_n], Ast0.DParams(lp,params,rp))) in
    k p

  and define_param p =
    let k p =
      rewrap p
	(match Ast0.unwrap p with
	  Ast0.DParam(id) -> let (n,id) = ident id in (n,Ast0.DParam(id))
	| Ast0.DParamEll(id,dots) ->
	    let (n,id) = ident id in
	    let (d,dots) = string_mcode dots in
	    (bind n d,Ast0.DParamEll(id,dots))
	| Ast0.MetaDParamList(name,lenname,constraints,pure) ->
	    let (n,name) = meta_mcode name in
	    (n,Ast0.MetaDParamList(name,lenname,constraints,pure))
	| Ast0.DPComma(comma) ->
	    let (n,comma) = string_mcode comma in (n,Ast0.DPComma(comma))
	| Ast0.DPdots(d) ->
	    let (n,d) = string_mcode d in (n,Ast0.DPdots(d))
	| Ast0.OptDParam(dp) ->
	    let (n,dp) = define_param dp in (n,Ast0.OptDParam(dp))) in
    k p

  and fninfo = function
      Ast0.FStorage(stg) ->
	let (n,stg) = storage_mcode stg in (n,Ast0.FStorage(stg))
    | Ast0.FType(ty) -> let (n,ty) = typeC ty in (n,Ast0.FType(ty))
    | Ast0.FInline(inline) ->
	let (n,inline) = string_mcode inline in (n,Ast0.FInline(inline))

  and attribute a =
    let k a =
      rewrap a
        (match Ast0.unwrap a with
          Ast0.Attribute(attr) ->
            let (attr_n,attr) = attr_arg attr in
            (attr_n,Ast0.Attribute(attr))
        | Ast0.GccAttribute(attr_,lp1,lp2,args,rp1,rp2) ->
            let (attr_n,attr_) = string_mcode attr_ in
            let (lp1_n,lp1) = string_mcode lp1 in
            let (lp2_n,lp2) = string_mcode lp2 in
	    let (args_n,args) = expression_dots args in
            let (rp1_n,rp1) = string_mcode rp1 in
            let (rp2_n,rp2) = string_mcode rp2 in
            (multibind [attr_n;lp1_n;lp2_n;args_n;rp1_n;rp2_n],
            Ast0.GccAttribute(attr_,lp1,lp2,args,rp1,rp2))
        | Ast0.CxxAttribute(lb1,args,rb1,rb2) ->
            let (lb1_n,lb1) = string_mcode lb1 in
	    let (args_n,args) = expression_dots args in
            let (rb1_n,rb1) = string_mcode rb1 in
            let (rb2_n,rb2) = string_mcode rb2 in
            (multibind [lb1_n;args_n;rb1_n;rb2_n],
            Ast0.CxxAttribute(lb1,args,rb1,rb2))
        | Ast0.CxxAttributeUsing(lb1,usng,atnm,dotdot,args,rb1,rb2) ->
            let (lb1_n,lb1) = string_mcode lb1 in
            let (usng_n,usng) = string_mcode usng in
	    let (atnm_n,atnm) = ident atnm in
            let (dotdot_n,dotdot) = string_mcode dotdot in
	    let (args_n,args) = expression_dots args in
            let (rb1_n,rb1) = string_mcode rb1 in
            let (rb2_n,rb2) = string_mcode rb2 in
            (multibind [lb1_n;usng_n;atnm_n;dotdot_n;args_n;rb1_n;rb2_n],
            Ast0.CxxAttributeUsing(lb1,usng,atnm,dotdot,args,rb1,rb2))) in
    attributefn all_functions k a

  and attr_arg a =
    let k a =
      rewrap a
        (match Ast0.unwrap a with
          Ast0.MetaAttr(name,constraints,pure) ->
            let (n,name) = meta_mcode name in
            (n,Ast0.MetaAttr(name,constraints,pure))
        | Ast0.MacroAttr (arg) ->
            let (args_n,arg) = string_mcode arg in (args_n,Ast0.MacroAttr(arg))
        | Ast0.MacroAttrArgs (attr,lp,args,rp) ->
            let (attr_n,attr) = string_mcode attr in
	    let (lp_n,lp) = string_mcode lp in
	    let (args_n,args) = expression_dots args in
	    let (rp_n,rp) = string_mcode rp in
            (multibind [attr_n;lp_n;args_n;rp_n], Ast0.MacroAttrArgs (attr,lp,args,rp))) in
    attr_argfn all_functions k a

  (* not parameterizable for now... *)
  and pragmainfo pi =
    let k pi =
      rewrap pi
	(match Ast0.unwrap pi with
	  Ast0.PragmaString(s) ->
	    let (s_n,s) = string_mcode s in
	    (s_n, Ast0.PragmaString(s))
	| Ast0.PragmaDots (dots) ->
	    let (dots_n,dots) = string_mcode dots in
	    (dots_n,Ast0.PragmaDots dots)
	| Ast0.MetaPragmaInfo (name, c, pure) ->
          let (name_n,name) = meta_mcode name in
          (name_n,Ast0.MetaPragmaInfo(name, c, pure))) in
    k pi

  (* not parameterisable, for now *)
  and directive d =
    let k d =
      rewrap d
	(match Ast0.unwrap d with
	  Ast0.Include(inc,name) ->
	    let (inc_n,inc) = string_mcode inc in
	    let (name_n,name) = inc_mcode name in
	    (bind inc_n name_n, Ast0.Include(inc,name))
	| Ast0.MetaInclude(inc,name) ->
	    let (inc_n,inc) = string_mcode inc in
	    let (name_n,name) = expression name in
	    (bind inc_n name_n, Ast0.MetaInclude(inc,name))
	| Ast0.Pragma(prg,id,body) ->
	    let (prg_n,prg) = string_mcode prg in
	    let (id_n,id) = ident id in
	    let (body_n,body) = pragmainfo body in
	    (multibind [prg_n;id_n;body_n],Ast0.Pragma(prg,id,body))
	| Ast0.UsingNamespace(usng,nmspc,name,sem) ->
	    let (usng_n,usng) = string_mcode usng in
	    let (nmspc_n,nmspc) = string_mcode nmspc in
	    let (name_n,name) = ident name in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [usng_n;nmspc_n],
             Ast0.UsingNamespace(usng,nmspc,name,sem))
	| Ast0.UsingTypename(usng,name,eq,tn,ty,sem) ->
	    let (usng_n,usng) = string_mcode usng in
	    let (name_n,name) = ident name in
	    let (eq_n,eq) = string_mcode eq in
	    let (tn_n,tn) = get_option string_mcode tn in
	    let (ty_n,ty) = typeC ty in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [usng_n;name_n;eq_n;tn_n;ty_n;sem_n],
             Ast0.UsingTypename(usng,name,eq,tn,ty,sem))
	| Ast0.UsingMember(usng,name,sem) ->
	    let (usng_n,usng) = string_mcode usng in
	    let (name_n,name) = ident name in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [usng_n;name_n;sem_n],Ast0.UsingMember(usng,name,sem))) in
    k d

  (* we only include the when string mcode w because the parameterised
     string_mcodefn function might have side-effects *)
  and whencode notfn alwaysfn = function
      Ast0.WhenNot (w,e,a) ->
	let (_,w) = string_mcode w in
	let (_,e) = string_mcode e in
	let (n,a) = notfn a in
	(n,Ast0.WhenNot(w,e,a))
    | Ast0.WhenAlways (w,e,a) ->
	let (_,w) = string_mcode w in
	let (_,e) = string_mcode e in
	let (n,a) = alwaysfn a in
	(n,Ast0.WhenAlways(w,e,a))
    | Ast0.WhenModifier(w,x) ->
	let (_,w) = string_mcode w in
	(option_default,Ast0.WhenModifier(w,x))
    | Ast0.WhenNotTrue(w,ee,e) ->
	let (_,w) = string_mcode w in
	let (_,ee) = string_mcode ee in
	let (n,e) = expression e in
	(n,Ast0.WhenNotTrue(w,ee,e))
    | Ast0.WhenNotFalse(w,ee,e) ->
	let (_,w) = string_mcode w in
	let (_,ee) = string_mcode ee in
	let (n,e) = expression e in
	(n,Ast0.WhenNotFalse(w,ee,e))

  (* for whencodes that do not have any of the above modifiers
   * returns (the new whencode expression, the updated whencode) *)
  and whencode_option cfn = function
    | Some (a,b,c) ->
	let (_,a2) = string_mcode a in
	let (_,b2) = string_mcode b in
	let (c1,c2) = cfn c in
	(c1, Some (a2,b2,c2))
    | None -> (option_default, None)

  and case_line c =
    let k c =
      rewrap c
	(match Ast0.unwrap c with
	  Ast0.Default(def,colon,code) ->
	    let (def_n,def) = string_mcode def in
	    let (colon_n,colon) = string_mcode colon in
	    let (code_n,code) = statement_dots code in
	    (multibind [def_n;colon_n;code_n], Ast0.Default(def,colon,code))
	| Ast0.Case(case,exp,colon,code) ->
	    let (case_n,case) = string_mcode case in
	    let (exp_n,exp) = expression exp in
	    let (colon_n,colon) = string_mcode colon in
	    let (code_n,code) = statement_dots code in
	    (multibind [case_n;exp_n;colon_n;code_n],
	     Ast0.Case(case,exp,colon,code))
	| Ast0.DisjCase(starter,case_lines,mids,ender) ->
	    do_disj starter case_lines mids ender case_line
	      (fun starter case_lines mids ender ->
		Ast0.DisjCase(starter,case_lines,mids,ender))
	| Ast0.OptCase(case) ->
	    let (n,case) = case_line case in (n,Ast0.OptCase(case))) in
    casefn all_functions k c

  and exec_code e =
    (* not configurable *)
    rewrap e
      (match Ast0.unwrap e with
	Ast0.ExecEval(colon,id) ->
	  let (colon_n,colon) = string_mcode colon in
	  let (id_n,id) = expression id in
	  (bind colon_n id_n,Ast0.ExecEval(colon,id))
      | Ast0.ExecToken(tok) ->
	  let (tok_n,tok) = string_mcode tok in
	  (tok_n,Ast0.ExecToken(tok))
      | Ast0.ExecDots(dots) ->
	  let (dots_n,dots) = string_mcode dots in
	  (dots_n,Ast0.ExecDots(dots)))

  and top_level t =
    let k t =
      rewrap t
	(match Ast0.unwrap t with
	  Ast0.FILEINFO(old_file,new_file) ->
	    let (old_file_n,old_file) = string_mcode old_file in
	    let (new_file_n,new_file) = string_mcode new_file in
	    (bind old_file_n new_file_n,Ast0.FILEINFO(old_file,new_file))
	| Ast0.NONDECL(statement_dots) ->
	    let (n,statement_dots) = statement statement_dots in
	    (n,Ast0.NONDECL(statement_dots))
	| Ast0.CODE(stmt_dots) ->
	    let (stmt_dots_n,stmt_dots) = statement_dots stmt_dots in
	    (stmt_dots_n, Ast0.CODE(stmt_dots))
	| Ast0.TOPCODE(stmt_dots) ->
	    let (stmt_dots_n,stmt_dots) = statement_dots stmt_dots in
	    (stmt_dots_n, Ast0.TOPCODE(stmt_dots))
	| Ast0.ERRORWORDS(exps) ->
	    let (n,exps) = map_split_bind expression exps in
	    (n, Ast0.ERRORWORDS(exps))
	| Ast0.OTHER(_) -> failwith "unexpected code") in
    topfn all_functions k t

  and anything a = (* for compile_iso, not parameterisable *)
    let k = function
	Ast0.DotsExprTag(exprs) ->
	  let (exprs_n,exprs) = expression_dots exprs in
	  (exprs_n,Ast0.DotsExprTag(exprs))
      | Ast0.DotsInitTag(inits) ->
	  let (inits_n,inits) = initialiser_dots inits in
	  (inits_n,Ast0.DotsInitTag(inits))
      | Ast0.DotsParamTag(params) ->
	  let (params_n,params) = parameter_dots params in
	  (params_n,Ast0.DotsParamTag(params))
      | Ast0.DotsTemplateParamTag(params) ->
	  let (tparams_n,tparams) = template_parameter_dots params in
	  (tparams_n,Ast0.DotsTemplateParamTag(tparams))
      | Ast0.DotsStmtTag(stmts) ->
	  let (stmts_n,stmts) = statement_dots stmts in
	  (stmts_n,Ast0.DotsStmtTag(stmts))
      | Ast0.DotsDeclTag(decls) ->
	  let (decls_n,decls) = declaration_dots decls in
	  (decls_n,Ast0.DotsDeclTag(decls))
      | Ast0.DotsFieldTag(decls) ->
	  let (decls_n,decls) = field_dots decls in
	  (decls_n,Ast0.DotsFieldTag(decls))
      | Ast0.DotsEnumDeclTag(decls) ->
	  let (decls_n,decls) = enum_decl_dots decls in
	  (decls_n,Ast0.DotsEnumDeclTag(decls))
      | Ast0.DotsCaseTag(cases) ->
	  let (cases_n,cases) = case_line_dots cases in
	  (cases_n,Ast0.DotsCaseTag(cases))
      | Ast0.DotsDefParamTag(dps) ->
	  let (dps_n,dps) = define_param_dots dps in
	  (dps_n,Ast0.DotsDefParamTag(dps))
      | Ast0.IdentTag(id) ->
	  let (id_n,id) = ident id in
	  (id_n,Ast0.IdentTag(id))
      | Ast0.ExprTag(exp) ->
	  let (exp_n,exp) = expression exp in
	  (exp_n,Ast0.ExprTag(exp))
      | Ast0.AssignOpTag(op) ->
	  let (op_n,op) = assignOp op in
	  (op_n,Ast0.AssignOpTag(op))
      | Ast0.BinaryOpTag(op) ->
	  let (op_n,op) = binaryOp op in
	  (op_n,Ast0.BinaryOpTag(op))
      | Ast0.ArgExprTag(exp) ->
	  let (exp_n,exp) = expression exp in
	  (exp_n,Ast0.ArgExprTag(exp))
      | Ast0.TestExprTag(exp) ->
	  let (exp_n,exp) = expression exp in
	  (exp_n,Ast0.TestExprTag(exp))
      | Ast0.TypeCTag(ty) ->
	  let (ty_n,ty) = typeC ty in
	  (ty_n,Ast0.TypeCTag(ty))
      | Ast0.ParamTag(param) ->
	  let (param_n,param) = parameterTypeDef param in
	  (param_n,Ast0.ParamTag(param))
      | Ast0.TemplateParamTag(param) ->
	  let (tparam_n,tparam) = templateParameterTypeDef param in
	  (tparam_n,Ast0.TemplateParamTag(tparam))
      | Ast0.InitTag(init) ->
	  let (init_n,init) = initialiser init in
	  (init_n,Ast0.InitTag(init))
      | Ast0.DeclTag(decl) ->
	  let (decl_n,decl) = declaration decl in
	  (decl_n,Ast0.DeclTag(decl))
      | Ast0.FieldTag(decl) ->
	  let (decl_n,decl) = field decl in
	  (decl_n,Ast0.FieldTag(decl))
      | Ast0.EnumDeclTag(decl) ->
	  let (decl_n,decl) = enum_decl decl in
	  (decl_n,Ast0.EnumDeclTag(decl))
      | Ast0.StmtTag(stmt) ->
	  let (stmt_n,stmt) = statement stmt in
	  (stmt_n,Ast0.StmtTag(stmt))
      | Ast0.ForInfoTag(fi) ->
	  let (fi_n,fi) = forinfo fi in
	  (fi_n,Ast0.ForInfoTag(fi))
      | Ast0.CaseLineTag(c) ->
	  let (c_n,c) = case_line c in
	  (c_n,Ast0.CaseLineTag(c))
      | Ast0.StringFragmentTag(f) ->
	  let (f_n,f) = string_fragment f in
	  (f_n,Ast0.StringFragmentTag(f))
      | Ast0.AttributeTag(a) ->
	  let (a_n,a) = attribute a in
	  (a_n,Ast0.AttributeTag(a))
      | Ast0.AttrArgTag(a) ->
	  let (a_n,a) = attr_arg a in
	  (a_n,Ast0.AttrArgTag(a))
      | Ast0.TopTag(top) ->
	  let (top_n,top) = top_level top in
	  (top_n,Ast0.TopTag(top))
      | Ast0.IsoWhenTag(x) -> (option_default,Ast0.IsoWhenTag(x))
      | Ast0.IsoWhenTTag(e) ->
	  let (e_n,e) = expression e in
	  (e_n,Ast0.IsoWhenTTag(e))
      | Ast0.IsoWhenFTag(e) ->
	  let (e_n,e) = expression e in
	  (e_n,Ast0.IsoWhenFTag(e))
      | Ast0.MetaPosTag(Ast0.MetaPos(nm,constraints,ct)) ->
       	 let (n,nm) = meta_mcode nm in
	   (n,Ast0.MetaPosTag(Ast0.MetaPos(nm,constraints,ct)))
      | Ast0.MetaPosTag(Ast0.MetaCom(nm,constraints)) ->
       	 let (n,nm) = meta_mcode nm in
	   (n,Ast0.MetaPosTag(Ast0.MetaCom(nm,constraints)))
      |	Ast0.HiddenVarTag(var) -> failwith "not supported"
      |	Ast0.WhenTag(a,e,b) -> anything b in
    k a

  (* not done for combiner, because the statement is assumed to be already
     represented elsewhere in the code *)

  and all_functions =
    {VT0.meta_mcode = meta_mcode;
      VT0.ident = ident;
      VT0.expression = expression;
      VT0.assignOp = assignOp;
      VT0.binaryOp = binaryOp;
      VT0.typeC = typeC;
      VT0.declaration = declaration;
      VT0.field = field;
      VT0.enum_decl = enum_decl;
      VT0.initialiser = initialiser;
      VT0.initialiser_list = initialiser_dots;
      VT0.parameter = parameterTypeDef;
      VT0.template_parameter = templateParameterTypeDef;
      VT0.parameter_list = parameter_dots;
      VT0.template_parameter_list = template_parameter_dots;
      VT0.statement = statement;
      VT0.forinfo = forinfo;
      VT0.case_line = case_line;
      VT0.define_param = define_param;
      VT0.string_fragment = string_fragment;
      VT0.attribute = attribute;
      VT0.attr_arg = attr_arg;
      VT0.top_level = top_level;
      VT0.expression_dots = expression_dots;
      VT0.statement_dots = statement_dots;
      VT0.declaration_dots = declaration_dots;
      VT0.field_dots = field_dots;
      VT0.enum_decl_dots = enum_decl_dots;
      VT0.case_line_dots = case_line_dots;
      VT0.define_param_dots = define_param_dots;
      VT0.anything = anything} in
  all_functions

let combiner_dz r =
    {VT0.combiner_rec_meta_mcode =
      (function e -> let (n,_) = r.VT0.meta_mcode e in n);
      VT0.combiner_rec_ident =
      (function e -> let (n,_) = r.VT0.ident e in n);
      VT0.combiner_rec_expression =
      (function e -> let (n,_) = r.VT0.expression e in n);
      VT0.combiner_rec_assignOp =
      (function e -> let (n,_) = r.VT0.assignOp e in n);
      VT0.combiner_rec_binaryOp =
      (function e -> let (n,_) = r.VT0.binaryOp e in n);
      VT0.combiner_rec_typeC =
      (function e -> let (n,_) = r.VT0.typeC e in n);
      VT0.combiner_rec_declaration =
      (function e -> let (n,_) = r.VT0.declaration e in n);
      VT0.combiner_rec_field =
      (function e -> let (n,_) = r.VT0.field e in n);
      VT0.combiner_rec_enumdecl =
      (function e -> let (n,_) = r.VT0.enum_decl e in n);
      VT0.combiner_rec_initialiser =
      (function e -> let (n,_) = r.VT0.initialiser e in n);
      VT0.combiner_rec_initialiser_list =
      (function e -> let (n,_) = r.VT0.initialiser_list e in n);
      VT0.combiner_rec_parameter =
      (function e -> let (n,_) = r.VT0.parameter e in n);
      VT0.combiner_rec_template_parameter =
      (function e -> let (n,_) = r.VT0.template_parameter e in n);
      VT0.combiner_rec_parameter_list =
      (function e -> let (n,_) = r.VT0.parameter_list e in n);
      VT0.combiner_rec_template_parameter_list =
      (function e -> let (n,_) = r.VT0.template_parameter_list e in n);
      VT0.combiner_rec_statement =
      (function e -> let (n,_) = r.VT0.statement e in n);
      VT0.combiner_rec_forinfo =
      (function e -> let (n,_) = r.VT0.forinfo e in n);
      VT0.combiner_rec_case_line =
      (function e -> let (n,_) = r.VT0.case_line e in n);
      VT0.combiner_rec_define_param =
      (function e -> let (n,_) = r.VT0.define_param e in n);
      VT0.combiner_rec_string_fragment =
      (function e -> let (n,_) = r.VT0.string_fragment e in n);
      VT0.combiner_rec_attribute =
      (function e -> let (n,_) = r.VT0.attribute e in n);
      VT0.combiner_rec_attr_arg =
      (function e -> let (n,_) = r.VT0.attr_arg e in n);
      VT0.combiner_rec_top_level =
      (function e -> let (n,_) = r.VT0.top_level e in n);
      VT0.combiner_rec_expression_dots =
      (function e -> let (n,_) = r.VT0.expression_dots e in n);
      VT0.combiner_rec_statement_dots =
      (function e -> let (n,_) = r.VT0.statement_dots e in n);
      VT0.combiner_rec_declaration_dots =
      (function e -> let (n,_) = r.VT0.declaration_dots e in n);
      VT0.combiner_rec_field_dots =
      (function e -> let (n,_) = r.VT0.field_dots e in n);
      VT0.combiner_rec_enum_decl_dots =
      (function e -> let (n,_) = r.VT0.enum_decl_dots e in n);
      VT0.combiner_rec_case_line_dots =
      (function e -> let (n,_) = r.VT0.case_line_dots e in n);
      VT0.combiner_rec_define_param_dots =
      (function e -> let (n,_) = r.VT0.define_param_dots e in n);
      VT0.combiner_rec_anything =
      (function e -> let (n,_) = r.VT0.anything e in n)}

type 'b cmcodefn = { cmcode: 'a. 'a Ast0.mcode -> 'b }
type 'b cdonothingfn =
    { cdonothing: 'a. 'b Visitor_ast0_types.combiner_rec_functions ->
      ('a Ast0.wrap -> 'b) -> 'a Ast0.wrap -> 'b }

let combiner bind option_default mcode donothing
    ?(meta_mcode=mcode.cmcode) ?(string_mcode=mcode.cmcode) ?(const_mcode=mcode.cmcode)
    ?(simpleAssign_mcode=mcode.cmcode) ?(opAssign_mcode=mcode.cmcode)
    ?(fixOp_mcode=mcode.cmcode) ?(unaryOp_mcode=mcode.cmcode) ?(arithOp_mcode=mcode.cmcode)
    ?(logicalOp_mcode=mcode.cmcode) ?(cv_mcode=mcode.cmcode) ?(sign_mcode=mcode.cmcode)
    ?(struct_mcode=mcode.cmcode) ?(storage_mcode=mcode.cmcode) ?(inc_mcode=mcode.cmcode)
    ?(dotsexpr=donothing.cdonothing) ?(dotsinit=donothing.cdonothing)
    ?(dotsparam=donothing.cdonothing) ?(dotstemplateparam=donothing.cdonothing)
    ?(dotsstmt=donothing.cdonothing) ?(dotsdecl=donothing.cdonothing)
    ?(dotsfield=donothing.cdonothing) ?(dotsenumdecl=donothing.cdonothing)
    ?(dotscase=donothing.cdonothing) ?(dotsdefpar=donothing.cdonothing)
    ?(ident=donothing.cdonothing) ?(expr=donothing.cdonothing)
    ?(assignOp=donothing.cdonothing) ?(binaryOp=donothing.cdonothing)
    ?(ty=donothing.cdonothing) ?(init=donothing.cdonothing)
    ?(param=donothing.cdonothing) ?(template_param=donothing.cdonothing)
    ?(decl=donothing.cdonothing) ?(field=donothing.cdonothing)
    ?(enumdecl=donothing.cdonothing) ?(stmt=donothing.cdonothing)
    ?(forinfo=donothing.cdonothing) ?(case=donothing.cdonothing)
    ?(string_fragment=donothing.cdonothing) ?(attribute=donothing.cdonothing)
    ?(attr_arg=donothing.cdonothing) ?(top=donothing.cdonothing) endarg =

  let dz = combiner_dz in
  let xk k e = let (n,_) = k e in n in
  combiner_dz (visitor COMBINER bind option_default
    (function mc -> (meta_mcode mc,mc))
    (function mc -> (string_mcode mc,mc))
    (function mc -> (const_mcode mc,mc))
    (function mc -> (simpleAssign_mcode mc,mc))
    (function mc -> (opAssign_mcode mc,mc))
    (function mc -> (fixOp_mcode mc,mc))
    (function mc -> (unaryOp_mcode mc,mc))
    (function mc -> (arithOp_mcode mc,mc))
    (function mc -> (logicalOp_mcode mc,mc))
    (function mc -> (cv_mcode mc,mc))
    (function mc -> (sign_mcode mc,mc))
    (function mc -> (struct_mcode mc,mc))
    (function mc -> (storage_mcode mc,mc))
    (function mc -> (inc_mcode mc,mc))
    (fun r k e -> (dotsexpr (dz r) (xk k) e, e))
    (fun r k e -> (dotsinit (dz r) (xk k) e, e))
    (fun r k e -> (dotsparam (dz r) (xk k) e, e))
    (fun r k e -> (dotstemplateparam (dz r) (xk k) e, e))
    (fun r k e -> (dotsstmt (dz r) (xk k) e, e))
    (fun r k e -> (dotsdecl (dz r) (xk k) e, e))
    (fun r k e -> (dotsfield (dz r) (xk k) e, e))
    (fun r k e -> (dotsenumdecl (dz r) (xk k) e, e))
    (fun r k e -> (dotscase (dz r) (xk k) e, e))
    (fun r k e -> (dotsdefpar (dz r) (xk k) e, e))
    (fun r k e -> (ident (dz r) (xk k) e, e))
    (fun r k e -> (expr (dz r) (xk k) e, e))
    (fun r k e -> (assignOp (dz r) (xk k) e, e))
    (fun r k e -> (binaryOp (dz r) (xk k) e, e))
    (fun r k e -> (ty (dz r) (xk k) e, e))
    (fun r k e -> (init (dz r) (xk k) e, e))
    (fun r k e -> (param (dz r) (xk k) e, e))
    (fun r k e -> (template_param (dz r) (xk k) e, e))
    (fun r k e -> (decl (dz r) (xk k) e, e))
    (fun r k e -> (field (dz r) (xk k) e, e))
    (fun r k e -> (enumdecl (dz r) (xk k) e, e))
    (fun r k e -> (stmt (dz r) (xk k) e, e))
    (fun r k e -> (forinfo (dz r) (xk k) e, e))
    (fun r k e -> (case (dz r) (xk k) e, e))
    (fun r k e -> (string_fragment (dz r) (xk k) e, e))
    (fun r k e -> (attribute (dz r) (xk k) e, e))
    (fun r k e -> (attr_arg (dz r) (xk k) e, e))
    (fun r k e -> (top (dz r) (xk k) e, e)))

let donothing r k e = k e

let combiner_default bind option_default
    ?(meta_mcode=(fun x -> option_default)) ?(string_mcode=(fun x -> option_default))
    ?(const_mcode=(fun x -> option_default)) ?(simpleAssign_mcode=(fun x -> option_default))
    ?(opAssign_mcode=(fun x -> option_default)) ?(fixOp_mcode=(fun x -> option_default))
    ?(unaryOp_mcode=(fun x -> option_default)) ?(arithOp_mcode=(fun x -> option_default))
    ?(logicalOp_mcode=(fun x -> option_default)) ?(cv_mcode=(fun x -> option_default))
    ?(sign_mcode=(fun x -> option_default)) ?(struct_mcode=(fun x -> option_default))
    ?(storage_mcode=(fun x -> option_default)) ?(inc_mcode=(fun x -> option_default))
    ?(dotsexpr=donothing) ?(dotsinit=donothing) ?(dotsparam=donothing)
    ?(dotstemplateparam=donothing) ?(dotsstmt=donothing) ?(dotsdecl=donothing)
    ?(dotsfield=donothing) ?(dotsenumdecl=donothing) ?(dotscase=donothing)
    ?(dotsdefpar=donothing) ?(ident=donothing) ?(expr=donothing) ?(assignOp=donothing)
    ?(binaryOp=donothing) ?(ty=donothing) ?(init=donothing) ?(param=donothing)
    ?(template_param=donothing) ?(decl=donothing) ?(field=donothing) ?(enumdecl=donothing)
    ?(stmt=donothing) ?(forinfo=donothing) ?(case=donothing) ?(string_fragment=donothing)
    ?(attribute=donothing) ?(attr_arg=donothing) ?(top=donothing) endarg =
  combiner bind option_default
    {cmcode = (fun x -> option_default)} {cdonothing = donothing}
    ~meta_mcode ~string_mcode ~const_mcode ~simpleAssign_mcode ~opAssign_mcode
    ~fixOp_mcode ~unaryOp_mcode ~arithOp_mcode ~logicalOp_mcode ~cv_mcode ~sign_mcode
    ~struct_mcode ~storage_mcode ~inc_mcode
    ~dotsexpr ~dotsinit ~dotsparam ~dotstemplateparam ~dotsstmt ~dotsdecl
    ~dotsfield ~dotsenumdecl ~dotscase ~dotsdefpar
    ~ident ~expr ~assignOp ~binaryOp ~ty ~init ~param ~template_param ~decl ~field
    ~enumdecl ~stmt ~forinfo ~case ~string_fragment ~attribute ~attr_arg ~top endarg

let rebuilder_dz r =
  {VT0.rebuilder_rec_meta_mcode =
      (function e -> let (_,e) = r.VT0.meta_mcode e in e);
      VT0.rebuilder_rec_ident =
      (function e -> let (_,e) = r.VT0.ident e in e);
      VT0.rebuilder_rec_expression =
        (function e -> let (_,e) = r.VT0.expression e in e);
      VT0.rebuilder_rec_assignOp =
        (function e -> let (_,e) = r.VT0.assignOp e in e);
      VT0.rebuilder_rec_binaryOp =
      (function e -> let (_,e) = r.VT0.binaryOp e in e);
      VT0.rebuilder_rec_typeC =
      (function e -> let (_,e) = r.VT0.typeC e in e);
      VT0.rebuilder_rec_declaration =
      (function e -> let (_,e) = r.VT0.declaration e in e);
      VT0.rebuilder_rec_field =
      (function e -> let (_,e) = r.VT0.field e in e);
      VT0.rebuilder_rec_enumdecl =
      (function e -> let (_,e) = r.VT0.enum_decl e in e);
      VT0.rebuilder_rec_initialiser =
      (function e -> let (_,e) = r.VT0.initialiser e in e);
      VT0.rebuilder_rec_initialiser_list =
      (function e -> let (_,e) = r.VT0.initialiser_list e in e);
      VT0.rebuilder_rec_parameter =
      (function e -> let (_,e) = r.VT0.parameter e in e);
      VT0.rebuilder_rec_template_parameter =
      (function e -> let (_,e) = r.VT0.template_parameter e in e);
      VT0.rebuilder_rec_parameter_list =
      (function e -> let (_,e) = r.VT0.parameter_list e in e);
      VT0.rebuilder_rec_template_parameter_list =
      (function e -> let (_,e) = r.VT0.template_parameter_list e in e);
      VT0.rebuilder_rec_statement =
      (function e -> let (_,e) = r.VT0.statement e in e);
      VT0.rebuilder_rec_forinfo =
      (function e -> let (_,e) = r.VT0.forinfo e in e);
      VT0.rebuilder_rec_case_line =
      (function e -> let (_,e) = r.VT0.case_line e in e);
      VT0.rebuilder_rec_string_fragment =
      (function e -> let (_,e) = r.VT0.string_fragment e in e);
      VT0.rebuilder_rec_attribute =
      (function e -> let (_,e) = r.VT0.attribute e in e);
      VT0.rebuilder_rec_attr_arg =
      (function e -> let (_,e) = r.VT0.attr_arg e in e);
      VT0.rebuilder_rec_top_level =
      (function e -> let (_,e) = r.VT0.top_level e in e);
      VT0.rebuilder_rec_expression_dots =
      (function e -> let (_,e) = r.VT0.expression_dots e in e);
      VT0.rebuilder_rec_statement_dots =
      (function e -> let (_,e) = r.VT0.statement_dots e in e);
      VT0.rebuilder_rec_declaration_dots =
      (function e -> let (_,e) = r.VT0.declaration_dots e in e);
      VT0.rebuilder_rec_field_dots =
      (function e -> let (_,e) = r.VT0.field_dots e in e);
      VT0.rebuilder_rec_enum_decl_dots =
      (function e -> let (_,e) = r.VT0.enum_decl_dots e in e);
      VT0.rebuilder_rec_case_line_dots =
      (function e -> let (_,e) = r.VT0.case_line_dots e in e);
      VT0.rebuilder_rec_define_param_dots =
      (function e -> let (_,e) = r.VT0.define_param_dots e in e);
      VT0.rebuilder_rec_anything =
      (function e -> let (_,e) = r.VT0.anything e in e)}

type rmcodefn = { rmcode: 'a. 'a Ast0.mcode -> 'a Ast0.mcode }
type rdonothingfn =
    { rdonothing: 'a. Visitor_ast0_types.rebuilder_rec_functions ->
      ('a Ast0.wrap -> 'a Ast0.wrap) -> 'a Ast0.wrap -> 'a Ast0.wrap }

let rebuilder mcode donothing
    ?(meta_mcode=mcode.rmcode) ?(string_mcode=mcode.rmcode) ?(const_mcode=mcode.rmcode)
    ?(simpleAssign_mcode=mcode.rmcode) ?(opAssign_mcode=mcode.rmcode)
    ?(fixOp_mcode=mcode.rmcode) ?(unaryOp_mcode=mcode.rmcode) ?(arithOp_mcode=mcode.rmcode)
    ?(logicalOp_mcode=mcode.rmcode) ?(cv_mcode=mcode.rmcode) ?(sign_mcode=mcode.rmcode)
    ?(struct_mcode=mcode.rmcode) ?(storage_mcode=mcode.rmcode) ?(inc_mcode=mcode.rmcode)
    ?(dotsexpr=donothing.rdonothing) ?(dotsinit=donothing.rdonothing)
    ?(dotsparam=donothing.rdonothing) ?(dotstemplateparam=donothing.rdonothing)
    ?(dotsstmt=donothing.rdonothing) ?(dotsdecl=donothing.rdonothing)
    ?(dotsfield=donothing.rdonothing) ?(dotsenumdecl=donothing.rdonothing)
    ?(dotscase=donothing.rdonothing) ?(dotsdefpar=donothing.rdonothing)
    ?(ident=donothing.rdonothing) ?(expr=donothing.rdonothing)
    ?(assignOp=donothing.rdonothing) ?(binaryOp=donothing.rdonothing)
    ?(ty=donothing.rdonothing) ?(init=donothing.rdonothing)
    ?(param=donothing.rdonothing) ?(template_param=donothing.rdonothing)
    ?(decl=donothing.rdonothing) ?(field=donothing.rdonothing)
    ?(enumdecl=donothing.rdonothing) ?(stmt=donothing.rdonothing)
    ?(forinfo=donothing.rdonothing) ?(case=donothing.rdonothing)
    ?(string_fragment=donothing.rdonothing) ?(attribute=donothing.rdonothing)
    ?(attr_arg=donothing.rdonothing) ?(top=donothing.rdonothing) endarg =

  let dz = rebuilder_dz in
  let xk k e = let (_,e) = k e in e in
  rebuilder_dz
    (visitor REBUILDER (fun x y -> x) ()
    (function mc -> ((),meta_mcode mc))
    (function mc -> ((),string_mcode mc))
    (function mc -> ((),const_mcode mc))
    (function mc -> ((),simpleAssign_mcode mc))
    (function mc -> ((),opAssign_mcode mc))
    (function mc -> ((),fixOp_mcode mc))
    (function mc -> ((),unaryOp_mcode mc))
    (function mc -> ((),arithOp_mcode mc))
    (function mc -> ((),logicalOp_mcode mc))
    (function mc -> ((),cv_mcode mc))
    (function mc -> ((),sign_mcode mc))
    (function mc -> ((),struct_mcode mc))
    (function mc -> ((),storage_mcode mc))
    (function mc -> ((),inc_mcode mc))
    (fun r k e -> ((),dotsexpr (dz r) (xk k) e))
    (fun r k e -> ((),dotsinit (dz r) (xk k) e))
    (fun r k e -> ((),dotsparam (dz r) (xk k) e))
    (fun r k e -> ((),dotstemplateparam (dz r) (xk k) e))
    (fun r k e -> ((),dotsstmt (dz r) (xk k) e))
    (fun r k e -> ((),dotsdecl (dz r) (xk k) e))
    (fun r k e -> ((),dotsfield (dz r) (xk k) e))
    (fun r k e -> ((),dotsenumdecl (dz r) (xk k) e))
    (fun r k e -> ((),dotscase (dz r) (xk k) e))
    (fun r k e -> ((),dotsdefpar (dz r) (xk k) e))
    (fun r k e -> ((),ident (dz r) (xk k) e))
    (fun r k e -> ((),expr (dz r) (xk k) e))
    (fun r k e -> ((),assignOp (dz r) (xk k) e))
    (fun r k e -> ((),binaryOp (dz r) (xk k) e))
    (fun r k e -> ((),ty (dz r) (xk k) e))
    (fun r k e -> ((),init (dz r) (xk k) e))
    (fun r k e -> ((),param (dz r) (xk k) e))
    (fun r k e -> ((),template_param (dz r) (xk k) e))
    (fun r k e -> ((),decl (dz r) (xk k) e))
    (fun r k e -> ((),field (dz r) (xk k) e))
    (fun r k e -> ((),enumdecl (dz r) (xk k) e))
    (fun r k e -> ((),stmt (dz r) (xk k) e))
    (fun r k e -> ((),forinfo (dz r) (xk k) e))
    (fun r k e -> ((),case (dz r) (xk k) e))
    (fun r k e -> ((),string_fragment (dz r) (xk k) e))
    (fun r k e -> ((),attribute (dz r) (xk k) e))
    (fun r k e -> ((),attr_arg (dz r) (xk k) e))
    (fun r k e -> ((),top (dz r) (xk k) e)))

let id x = x

let rebuilder_default
    ?(meta_mcode=id) ?(string_mcode=id) ?(const_mcode=id) ?(simpleAssign_mcode=id)
    ?(opAssign_mcode=id) ?(fixOp_mcode=id) ?(unaryOp_mcode=id) ?(arithOp_mcode=id)
    ?(logicalOp_mcode=id) ?(cv_mcode=id) ?(sign_mcode=id) ?(struct_mcode=id)
    ?(storage_mcode=id) ?(inc_mcode=id) ?(dotsexpr=donothing) ?(dotsinit=donothing)
    ?(dotsparam=donothing) ?(dotstemplateparam=donothing) ?(dotsstmt=donothing)
    ?(dotsdecl=donothing) ?(dotsfield=donothing) ?(dotsenumdecl=donothing)
    ?(dotscase=donothing) ?(dotsdefpar=donothing) ?(ident=donothing)
    ?(expr=donothing) ?(assignOp=donothing) ?(binaryOp=donothing) ?(ty=donothing)
    ?(init=donothing) ?(param=donothing) ?(template_param=donothing) ?(decl=donothing)
    ?(field=donothing) ?(enumdecl=donothing) ?(stmt=donothing) ?(forinfo=donothing)
    ?(case=donothing) ?(string_fragment=donothing) ?(attribute=donothing)
    ?(attr_arg=donothing) ?(top=donothing) endarg =
  rebuilder {rmcode = (fun x -> x)} {rdonothing = (fun r k e -> k e)}
    ~meta_mcode ~string_mcode ~const_mcode ~simpleAssign_mcode ~opAssign_mcode
    ~fixOp_mcode ~unaryOp_mcode ~arithOp_mcode ~logicalOp_mcode ~cv_mcode ~sign_mcode
    ~struct_mcode ~storage_mcode ~inc_mcode
    ~dotsexpr ~dotsinit ~dotsparam ~dotstemplateparam ~dotsstmt ~dotsdecl
    ~dotsfield ~dotsenumdecl ~dotscase ~dotsdefpar
    ~ident ~expr ~assignOp ~binaryOp ~ty ~init ~param ~template_param ~decl ~field
    ~enumdecl ~stmt ~forinfo ~case ~string_fragment ~attribute ~attr_arg ~top endarg

type 'b crmcodefn = { crmcode: 'a. 'a Ast0.mcode -> 'b * 'a Ast0.mcode }
type 'b crdonothingfn =
    { crdonothing: 'a. 'b Visitor_ast0_types.all_functions ->
      ('a Ast0.wrap -> 'b * 'a Ast0.wrap) -> 'a Ast0.wrap -> 'b * 'a Ast0.wrap }

let combiner_rebuilder bind option_default mcode donothing
    ?(meta_mcode=mcode.crmcode) ?(string_mcode=mcode.crmcode) ?(const_mcode=mcode.crmcode)
    ?(simpleAssign_mcode=mcode.crmcode) ?(opAssign_mcode=mcode.crmcode)
    ?(fixOp_mcode=mcode.crmcode) ?(unaryOp_mcode=mcode.crmcode) ?(arithOp_mcode=mcode.crmcode)
    ?(logicalOp_mcode=mcode.crmcode) ?(cv_mcode=mcode.crmcode) ?(sign_mcode=mcode.crmcode)
    ?(struct_mcode=mcode.crmcode) ?(storage_mcode=mcode.crmcode) ?(inc_mcode=mcode.crmcode)
    ?(dotsexpr=donothing.crdonothing) ?(dotsinit=donothing.crdonothing)
    ?(dotsparam=donothing.crdonothing) ?(dotstemplateparam=donothing.crdonothing)
    ?(dotsstmt=donothing.crdonothing) ?(dotsdecl=donothing.crdonothing)
    ?(dotsfield=donothing.crdonothing) ?(dotsenumdecl=donothing.crdonothing)
    ?(dotscase=donothing.crdonothing) ?(dotsdefpar=donothing.crdonothing)
    ?(ident=donothing.crdonothing) ?(expr=donothing.crdonothing)
    ?(assignOp=donothing.crdonothing) ?(binaryOp=donothing.crdonothing)
    ?(ty=donothing.crdonothing) ?(init=donothing.crdonothing)
    ?(param=donothing.crdonothing) ?(template_param=donothing.crdonothing)
    ?(decl=donothing.crdonothing) ?(field=donothing.crdonothing)
    ?(enumdecl=donothing.crdonothing) ?(stmt=donothing.crdonothing)
    ?(forinfo=donothing.crdonothing) ?(case=donothing.crdonothing)
    ?(string_fragment=donothing.crdonothing) ?(attribute=donothing.crdonothing)
    ?(attr_arg=donothing.crdonothing) ?(top=donothing.crdonothing) endarg =

  visitor BOTH bind option_default
    meta_mcode string_mcode const_mcode simpleAssign_mcode opAssign_mcode
    fixOp_mcode unaryOp_mcode arithOp_mcode logicalOp_mcode cv_mcode sign_mcode
    struct_mcode storage_mcode inc_mcode
    dotsexpr dotsinit dotsparam dotstemplateparam dotsstmt dotsdecl
    dotsfield dotsenumdecl dotscase dotsdefpar
    ident expr assignOp binaryOp ty init param template_param
    decl field enumdecl stmt forinfo case string_fragment
    attribute attr_arg top

let combiner_rebuilder_default bind option_default
    ?(meta_mcode=(fun x -> (option_default,x)))
    ?(string_mcode=(fun x -> (option_default,x)))
    ?(const_mcode=(fun x -> (option_default,x)))
    ?(simpleAssign_mcode=(fun x -> (option_default,x)))
    ?(opAssign_mcode=(fun x -> (option_default,x)))
    ?(fixOp_mcode=(fun x -> (option_default,x)))
    ?(unaryOp_mcode=(fun x -> (option_default,x)))
    ?(arithOp_mcode=(fun x -> (option_default,x)))
    ?(logicalOp_mcode=(fun x -> (option_default,x)))
    ?(cv_mcode=(fun x -> (option_default,x)))
    ?(sign_mcode=(fun x -> (option_default,x)))
    ?(struct_mcode=(fun x -> (option_default,x)))
    ?(storage_mcode=(fun x -> (option_default,x)))
    ?(inc_mcode=(fun x -> (option_default,x)))
    ?(dotsexpr=donothing) ?(dotsinit=donothing) ?(dotsparam=donothing)
    ?(dotstemplateparam=donothing) ?(dotsstmt=donothing) ?(dotsdecl=donothing)
    ?(dotsfield=donothing) ?(dotsenumdecl=donothing) ?(dotscase=donothing)
    ?(dotsdefpar=donothing) ?(ident=donothing) ?(expr=donothing)
    ?(assignOp=donothing) ?(binaryOp=donothing) ?(ty=donothing)
    ?(init=donothing) ?(param=donothing) ?(template_param=donothing)
    ?(decl=donothing) ?(field=donothing) ?(enumdecl=donothing)
    ?(stmt=donothing) ?(forinfo=donothing) ?(case=donothing)
    ?(string_fragment=donothing) ?(attribute=donothing)
    ?(attr_arg=donothing) ?(top=donothing) endarg =

  visitor BOTH bind option_default
    meta_mcode string_mcode const_mcode simpleAssign_mcode opAssign_mcode
    fixOp_mcode unaryOp_mcode arithOp_mcode logicalOp_mcode cv_mcode sign_mcode
    struct_mcode storage_mcode inc_mcode
    dotsexpr dotsinit dotsparam dotstemplateparam dotsstmt dotsdecl
    dotsfield dotsenumdecl dotscase dotsdefpar
    ident expr assignOp binaryOp ty init param template_param
    decl field enumdecl stmt forinfo case string_fragment
    attribute attr_arg top
