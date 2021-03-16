(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
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
    dotsexprfn dotsinitfn dotsparamfn dotsstmtfn dotsdeclfn dotsfieldfn
    dotsenumdeclfn dotscasefn dotsdefparfn
    identfn exprfn assignOpfn binaryOpfn tyfn initfn paramfn declfn fieldfn
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
	| Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
	    let (exp1_n,exp1) = expression exp1 in
	    let (lb_n,lb) = string_mcode lb in
	    let (exp2_n,exp2) = expression exp2 in
	    let (rb_n,rb) = string_mcode rb in
	    (multibind [exp1_n;lb_n;exp2_n;rb_n],
	     Ast0.ArrayAccess(exp1,lb,exp2,rb))
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
	| Ast0.Cast(lp,ty,attr,rp,exp) ->
	    let (lp_n,lp) = string_mcode lp in
	    let (ty_n,ty) = typeC ty in
	    let (attr_n,attr) = map_split_bind attribute attr in
	    let (rp_n,rp) = string_mcode rp in
	    let (exp_n,exp) = expression exp in
            (multibind [lp_n;ty_n;attr_n;rp_n;exp_n],
             Ast0.Cast(lp,ty,attr,rp,exp))
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
	  Ast0.ConstVol(cv,ty) ->
	    let (cv_n,cv) = cv_mcode cv in
	    let (ty_n,ty) = typeC ty in
	    let front =
	      (* bind in the right order *)
	      match Ast0.unwrap ty with
		Ast0.Pointer(ty,star) -> bind ty_n cv_n
	      |	_ -> bind cv_n ty_n in
	    (front, Ast0.ConstVol(cv,ty))
	| Ast0.BaseType(ty,strings) ->
	    let (strings_n,strings) = map_split_bind string_mcode strings in
	    (strings_n, Ast0.BaseType(ty,strings))
	| Ast0.Signed(sign,ty) ->
	    let (sign_n,sign) = sign_mcode sign in
	    let (ty_n,ty) = get_option typeC ty in
	    (bind sign_n ty_n, Ast0.Signed(sign,ty))
	| Ast0.Pointer(ty,star) ->
	    let (ty_n,ty) = typeC ty in
	    let (star_n,star) = string_mcode star in
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
	| Ast0.EnumName(kind,name) ->
	    let (kind_n,kind) = string_mcode kind in
	    let (name_n,name) = get_option ident name in
	    (bind kind_n name_n, Ast0.EnumName(kind,name))
	| Ast0.EnumDef(ty,lb,ids,rb) ->
	    let (ty_n,ty) = typeC ty in
	    let (lb_n,lb) = string_mcode lb in
	    let (ids_n,ids) = enum_decl_dots ids in
	    let (rb_n,rb) = string_mcode rb in
	    (multibind [ty_n;lb_n;ids_n;rb_n], Ast0.EnumDef(ty,lb,ids,rb))
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
	| Ast0.TypeName(name) ->
	    let (name_n,name) = string_mcode name in
	    (name_n,Ast0.TypeName(name))
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
	    (bind ty_n asty_n, Ast0.AsType(ty,asty))) in
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
              let (star_n,star) = string_mcode star in
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
             ((bind ty_n id_n, ty), id)

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
              let (star_n,star) = string_mcode star in
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

  and declaration d =
    let k d =
      rewrap d
	(match Ast0.unwrap d with
	  Ast0.MetaDecl(name,constraints,pure) ->
	    let (n,name) = meta_mcode name in
	    (n,Ast0.MetaDecl(name,constraints,pure))
	| Ast0.Init(stg,ty,id,attr,eq,ini,sem) ->
	    let (stg_n,stg) = get_option storage_mcode stg in
	    let ((ty_id_n,ty),id) = named_type ty id in
	    let (attr_n,attr) = map_split_bind attribute attr in
	    let (eq_n,eq) = string_mcode eq in
	    let (ini_n,ini) = initialiser ini in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [stg_n;ty_id_n;attr_n;eq_n;ini_n;sem_n],
	     Ast0.Init(stg,ty,id,attr,eq,ini,sem))
	| Ast0.UnInit(stg,ty,id,attr,sem) ->
	    let (stg_n,stg) = get_option storage_mcode stg in
	    let ((ty_id_n,ty),id) = named_type ty id in
	    let (attr_n,attr) = map_split_bind attribute attr in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [stg_n;ty_id_n;attr_n;sem_n],
	     Ast0.UnInit(stg,ty,id,attr,sem))
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
	| Ast0.MacroDecl(stg,name,lp,args,rp,attr,sem) ->
	    let (stg_n,stg) = get_option storage_mcode stg in
	    let (name_n,name) = ident name in
	    let (lp_n,lp) = string_mcode lp in
	    let (args_n,args) = expression_dots args in
	    let (rp_n,rp) = string_mcode rp in
	    let (attr_n,attr) = map_split_bind attribute attr in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [stg_n;name_n;lp_n;args_n;rp_n;attr_n;sem_n],
	     Ast0.MacroDecl(stg,name,lp,args,rp,attr,sem))
	| Ast0.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) ->
	    let (stg_n,stg) = get_option storage_mcode stg in
	    let (name_n,name) = ident name in
	    let (lp_n,lp) = string_mcode lp in
	    let (args_n,args) = expression_dots args in
	    let (rp_n,rp) = string_mcode rp in
	    let (eq_n,eq) = string_mcode eq in
	    let (ini_n,ini) = initialiser ini in
	    let (sem_n,sem) = string_mcode sem in
	    (multibind [stg_n;name_n;lp_n;args_n;rp_n;eq_n;ini_n;sem_n],
	     Ast0.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem))
	| Ast0.TyDecl(ty,attr,sem) ->
	    let (ty_n,ty) = typeC ty in
	    let (attr_n,attr) = map_split_bind attribute attr in
	    let (sem_n,sem) = string_mcode sem in
            (multibind [ty_n; attr_n; sem_n], Ast0.TyDecl(ty,attr,sem))
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
	| Ast0.Field(ty,id,bf,sem) ->
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
	    let (sem_n,sem) = string_mcode sem in
	    (multibind ([ty_id_n] @ bf_n @ [sem_n]), Ast0.Field(ty,id,bf,sem))
	| Ast0.DisjField(starter,decls,mids,ender) ->
	    do_disj starter decls mids ender field
	      (fun starter decls mids ender ->
		Ast0.DisjField(starter,decls,mids,ender))
	| Ast0.ConjField(starter,decls,mids,ender) ->
	    do_disj starter decls mids ender field
	      (fun starter decls mids ender ->
		Ast0.ConjField(starter,decls,mids,ender))
	| Ast0.Fdots(dots,whencode) ->
	    let (dots_n,dots) = string_mcode dots in
	    let (whencode_n, whencode) = match whencode with
              | Some (a,b,c) ->
                  let (_,a2) = string_mcode a in
                  let (_,b2) = string_mcode b in
                  let (c1,c2) = field c in (c1, Some (a2,b2,c2))
              | None -> (option_default, None) in
	    (bind dots_n whencode_n, Ast0.Fdots(dots,whencode))
	| Ast0.OptField(decl) ->
	    let (n,decl) = field decl in (n,Ast0.OptField(decl))) in
    fieldfn all_functions k d

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
	    let (whencode_n, whencode) = match whencode with
              | Some (a,b,c) ->
                  let (_,a2) = string_mcode a in
                  let (_,b2) = string_mcode b in
                  let (c1,c2) = enum_decl c in (c1, Some (a2,b2,c2))
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
	    let (whencode_n, whencode) = match whencode with
	      | Some (a,b,c) ->
		  let (_,a2) = string_mcode a in
		  let (_,b2) = string_mcode b in
		  let (c1,c2) = initialiser c in (c1, Some (a2,b2,c2))
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
	  Ast0.VoidParam(ty, attrs) ->
	    let (ty_n,ty) = typeC ty in
	    let (attr_n,attr) = map_split_bind attribute attrs in
            (bind ty_n attr_n,Ast0.VoidParam(ty, attrs))
	| Ast0.Param(ty,Some id,attrs) ->
	    let ((ty_id_n,ty),id) = named_type ty id in
	    let (attr_n,attr) = map_split_bind attribute attrs in
	    (bind ty_id_n attr_n, Ast0.Param(ty,Some id,attr))
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
	  Ast0.FunDecl(bef,fi,name,lp,params,va,rp,lbrace,body,rbrace,aft) ->
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
	    let (lbrace_n,lbrace) = string_mcode lbrace in
	    let (body_n,body) = statement_dots body in
	    let (rbrace_n,rbrace) = string_mcode rbrace in
	    (multibind
	       [fi_n;name_n;lp_n;params_n;va_n;rp_n;lbrace_n;body_n;rbrace_n],
	     Ast0.FunDecl(bef,fi,name,lp,params,va,rp,lbrace,body,rbrace,aft))
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
	| Ast0.While(whl,lp,exp,rp,body,aft) ->
	    let (whl_n,whl) = string_mcode whl in
	    let (lp_n,lp) = string_mcode lp in
	    let (exp_n,exp) = expression exp in
	    let (rp_n,rp) = string_mcode rp in
	    let (body_n,body) = statement body in
	    (multibind [whl_n;lp_n;exp_n;rp_n;body_n],
	     Ast0.While(whl,lp,exp,rp,body,aft))
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
	| Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,aft) ->
	    let (fr_n,fr) = string_mcode fr in
	    let (lp_n,lp) = string_mcode lp in
	    let (first_n,first) = forinfo first in
	    let (e2_n,e2) = get_option expression e2 in
	    let (sem2_n,sem2) = string_mcode sem2 in
	    let (e3_n,e3) = get_option expression e3 in
	    let (rp_n,rp) = string_mcode rp in
	    let (body_n,body) = statement body in
	    (multibind [fr_n;lp_n;first_n;e2_n;sem2_n;e3_n;rp_n;body_n],
	     Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,aft))
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
	| Ast0.Include(inc,name) ->
	    let (inc_n,inc) = string_mcode inc in
	    let (name_n,name) = inc_mcode name in
	    (bind inc_n name_n, Ast0.Include(inc,name))
	| Ast0.MetaInclude(inc,name) ->
	    let (inc_n,inc) = string_mcode inc in
	    let (name_n,name) = expression name in
	    (bind inc_n name_n, Ast0.MetaInclude(inc,name))
	| Ast0.Undef(def,id) ->
	    let (def_n,def) = string_mcode def in
	    let (id_n,id) = ident id in
	    (multibind [def_n;id_n],Ast0.Undef(def,id))
	| Ast0.Define(def,id,params,body) ->
	    let (def_n,def) = string_mcode def in
	    let (id_n,id) = ident id in
	    let (params_n,params) = define_parameters params in
	    let (body_n,body) = statement_dots body in
	    (multibind [def_n;id_n;params_n;body_n],
	     Ast0.Define(def,id,params,body))
	| Ast0.Pragma(prg,id,body) ->
	    let (prg_n,prg) = string_mcode prg in
	    let (id_n,id) = ident id in
	    let (body_n,body) = pragmainfo body in
	    (multibind [prg_n;id_n;body_n],Ast0.Pragma(prg,id,body))
	| Ast0.OptStm(re) ->
	    let (re_n,re) = statement re in (re_n,Ast0.OptStm(re))
	| Ast0.AsStmt(stm,asstm) ->
	    let (stm_n,stm) = statement stm in
	    let (asstm_n,asstm) = statement asstm in
	    (bind stm_n asstm_n, Ast0.AsStmt(stm,asstm))) in
    let (n,s) = stmtfn all_functions k s in
    (n,if mode = REBUILDER then process_bef_aft s else s)

  and forinfo fi =
    let k fi =
      rewrap fi
	(match Ast0.unwrap fi with
	  Ast0.ForExp(e1,sem1) ->
	    let (e1_n,e1) = get_option expression e1 in
	    let (sem1_n,sem1) = string_mcode sem1 in
	    (bind e1_n sem1_n, Ast0.ForExp(e1,sem1))
	| Ast0.ForDecl (bef,decl) ->
	    let (decl_n,decl) = declaration decl in
	    (decl_n,Ast0.ForDecl (bef,decl))) in
    forinfofn all_functions k fi

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
	    (dots_n,Ast0.PragmaDots dots)) in
    k pi

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
    | Ast0.FAttr(init) ->
	let (n,init) = attribute init in (n,Ast0.FAttr(init))

  and attribute a =
    let k a =
      rewrap a
        (match Ast0.unwrap a with
          Ast0.Attribute(attr) ->
            let (attr_n,attr) = attr_arg attr in
            (attr_n,Ast0.Attribute(attr))) in
              attributefn all_functions k a

  and attr_arg a =
    let k a =
      rewrap a
        (match Ast0.unwrap a with
          Ast0.MetaAttr(name,constraints,pure) ->
            let (n,name) = meta_mcode name in
            (n,Ast0.MetaAttr(name,constraints,pure))
        | Ast0.AttrName (arg) ->
            let (args_n,arg) = string_mcode arg in (args_n,Ast0.AttrName(arg))) in
              attr_argfn all_functions k a

  (* we only include the when string mcode w because the parameterised
     string_mcodefn function might have side-effects *)
  and whencode notfn alwaysfn = function
      Ast0.WhenNot (w,e,a) ->
	let (_,w) = string_mcode w in
	let (_,e) = string_mcode e in
	let (n,a) = notfn a in (n,Ast0.WhenNot(w,e,a))
    | Ast0.WhenAlways (w,e,a) ->
	let (_,w) = string_mcode w in
	let (_,e) = string_mcode e in
	let (n,a) = alwaysfn a in (n,Ast0.WhenAlways(w,e,a))
    | Ast0.WhenModifier(w,x) ->
	let (_,w) = string_mcode w in
	(option_default,Ast0.WhenModifier(w,x))
    | Ast0.WhenNotTrue(w,ee,e) ->
	let (_,w) = string_mcode w in
	let (_,ee) = string_mcode ee in
	let (n,e) = expression e in (n,Ast0.WhenNotTrue(w,ee,e))
    | Ast0.WhenNotFalse(w,ee,e) ->
	let (_,w) = string_mcode w in
	let (_,ee) = string_mcode ee in
	let (n,e) = expression e in (n,Ast0.WhenNotFalse(w,ee,e))

  (* for whencodes that do not have any of the above modifiers
   * returns (the new whencode expression, the updated whencode) *)
  and whencode_option cfn = function
    | Some (a,b,c) ->
	let (_,a2) = string_mcode a in
	let (_,b2) = string_mcode b in
	let (c1,c2) = cfn c in (c1, Some (a2,b2,c2))
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
      VT0.parameter_list = parameter_dots;
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

let combiner_functions =
  {VT0.combiner_meta_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_string_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_const_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_simpleAssign_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_opAssign_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_fix_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_unary_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_arithOp_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_logicalOp_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_cv_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_sign_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_struct_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_storage_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_inc_mcode = (fun opt_default mc -> opt_default);
   VT0.combiner_dotsexprfn = (fun r k e -> k e);
   VT0.combiner_dotsinitfn = (fun r k e -> k e);
   VT0.combiner_dotsparamfn = (fun r k e -> k e);
   VT0.combiner_dotsstmtfn = (fun r k e -> k e);
   VT0.combiner_dotsdeclfn = (fun r k e -> k e);
   VT0.combiner_dotsfieldfn = (fun r k e -> k e);
   VT0.combiner_dotsenumdeclfn = (fun r k e -> k e);
   VT0.combiner_dotscasefn = (fun r k e -> k e);
   VT0.combiner_dotsdefparfn = (fun r k e -> k e);
   VT0.combiner_identfn = (fun r k e -> k e);
   VT0.combiner_exprfn = (fun r k e -> k e);
   VT0.combiner_assignOpfn = (fun r k e -> k e);
   VT0.combiner_binaryOpfn = (fun r k e -> k e);
   VT0.combiner_tyfn = (fun r k e -> k e);
   VT0.combiner_initfn = (fun r k e -> k e);
   VT0.combiner_paramfn = (fun r k e -> k e);
   VT0.combiner_declfn = (fun r k e -> k e);
   VT0.combiner_fieldfn = (fun r k e -> k e);
   VT0.combiner_enumdeclfn = (fun r k e -> k e);
   VT0.combiner_stmtfn = (fun r k e -> k e);
   VT0.combiner_forinfofn = (fun r k e -> k e);
   VT0.combiner_casefn = (fun r k e -> k e);
   VT0.combiner_string_fragmentfn = (fun r k e -> k e);
   VT0.combiner_attributefn = (fun r k e -> k e);
   VT0.combiner_attr_argfn = (fun r k e -> k e);
   VT0.combiner_topfn = (fun r k e -> k e)}

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
      VT0.combiner_rec_parameter_list =
      (function e -> let (n,_) = r.VT0.parameter_list e in n);
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

let combiner bind option_default functions =
  let xk k e = let (n,_) = k e in n in
  let dz = combiner_dz in
  combiner_dz
    (visitor COMBINER bind option_default
    (function mc -> (functions.VT0.combiner_meta_mcode option_default mc,mc))
    (function mc -> (functions.VT0.combiner_string_mcode option_default mc,mc))
    (function mc -> (functions.VT0.combiner_const_mcode option_default mc,mc))
    (function mc ->
      (functions.VT0.combiner_simpleAssign_mcode option_default mc,mc))
    (function mc ->
      (functions.VT0.combiner_opAssign_mcode option_default mc,mc))
    (function mc -> (functions.VT0.combiner_fix_mcode option_default mc,mc))
    (function mc -> (functions.VT0.combiner_unary_mcode option_default mc,mc))
    (function mc ->
      (functions.VT0.combiner_arithOp_mcode option_default mc,mc))
    (function mc ->
      (functions.VT0.combiner_logicalOp_mcode option_default mc,mc))
    (function mc -> (functions.VT0.combiner_cv_mcode option_default mc,mc))
    (function mc -> (functions.VT0.combiner_sign_mcode option_default mc,mc))
    (function mc -> (functions.VT0.combiner_struct_mcode option_default mc,mc))
    (function mc ->
    (functions.VT0.combiner_storage_mcode option_default mc,mc))
    (function mc -> (functions.VT0.combiner_inc_mcode option_default mc,mc))
    (fun r k e -> (functions.VT0.combiner_dotsexprfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_dotsinitfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_dotsparamfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_dotsstmtfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_dotsdeclfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_dotsfieldfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_dotsenumdeclfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_dotscasefn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_dotsdefparfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_identfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_exprfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_assignOpfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_binaryOpfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_tyfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_initfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_paramfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_declfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_fieldfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_enumdeclfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_stmtfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_forinfofn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_casefn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_string_fragmentfn (dz r) (xk k) e,e))
    (fun r k e -> (functions.VT0.combiner_attributefn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_attr_argfn (dz r) (xk k) e, e))
    (fun r k e -> (functions.VT0.combiner_topfn (dz r) (xk k) e, e)))

let flat_combiner bind option_default
    meta_mcode string_mcode const_mcode simpleAssign_mcode opAssign_mcode
    fix_mcode unary_mcode arithOp_mcode logicalOp_mcode cv_mcode sign_mcode
    struct_mcode storage_mcode inc_mcode
    dotsexprfn dotsinitfn dotsparamfn dotsstmtfn dotsdeclfn dotsfieldfn
    dotsenumdeclfn dotscasefn dotsdefparfn
    identfn exprfn assignOpfn binaryOpfn tyfn initfn paramfn declfn fieldfn
    enumdeclfn
    stmtfn forinfofn casefn string_fragmentfn attributefn attr_argfn topfn =
  let dz = combiner_dz in
  let xk k e = let (n,_) = k e in n in
  combiner_dz (visitor COMBINER bind option_default
    (function mc -> (meta_mcode mc,mc))
    (function mc -> (string_mcode mc,mc))
    (function mc -> (const_mcode mc,mc))
    (function mc -> (simpleAssign_mcode mc,mc))
    (function mc -> (opAssign_mcode mc,mc))
    (function mc -> (fix_mcode mc,mc))
    (function mc -> (unary_mcode mc,mc))
    (function mc -> (arithOp_mcode mc,mc))
    (function mc -> (logicalOp_mcode mc,mc))
    (function mc -> (cv_mcode mc,mc))
    (function mc -> (sign_mcode mc,mc))
    (function mc -> (struct_mcode mc,mc))
    (function mc -> (storage_mcode mc,mc))
    (function mc -> (inc_mcode mc,mc))
    (fun r k e -> (dotsexprfn (dz r) (xk k) e, e))
    (fun r k e -> (dotsinitfn (dz r) (xk k) e, e))
    (fun r k e -> (dotsparamfn (dz r) (xk k) e, e))
    (fun r k e -> (dotsstmtfn (dz r) (xk k) e, e))
    (fun r k e -> (dotsdeclfn (dz r) (xk k) e, e))
    (fun r k e -> (dotsfieldfn (dz r) (xk k) e, e))
    (fun r k e -> (dotsenumdeclfn (dz r) (xk k) e, e))
    (fun r k e -> (dotscasefn (dz r) (xk k) e, e))
    (fun r k e -> (dotsdefparfn (dz r) (xk k) e, e))
    (fun r k e -> (identfn (dz r) (xk k) e, e))
    (fun r k e -> (exprfn (dz r) (xk k) e, e))
    (fun r k e -> (assignOpfn (dz r) (xk k) e, e))
    (fun r k e -> (binaryOpfn (dz r) (xk k) e, e))
    (fun r k e -> (tyfn (dz r) (xk k) e, e))
    (fun r k e -> (initfn (dz r) (xk k) e, e))
    (fun r k e -> (paramfn (dz r) (xk k) e, e))
    (fun r k e -> (declfn (dz r) (xk k) e, e))
    (fun r k e -> (fieldfn (dz r) (xk k) e, e))
    (fun r k e -> (enumdeclfn (dz r) (xk k) e, e))
    (fun r k e -> (stmtfn (dz r) (xk k) e, e))
    (fun r k e -> (forinfofn (dz r) (xk k) e, e))
    (fun r k e -> (casefn (dz r) (xk k) e, e))
    (fun r k e -> (string_fragmentfn (dz r) (xk k) e, e))
    (fun r k e -> (attributefn (dz r) (xk k) e, e))
    (fun r k e -> (attr_argfn (dz r) (xk k) e, e))
    (fun r k e -> (topfn (dz r) (xk k) e, e)))

let rebuilder_functions =
  {VT0.rebuilder_meta_mcode = (fun mc -> mc);
   VT0.rebuilder_string_mcode = (fun mc -> mc);
   VT0.rebuilder_const_mcode = (fun mc -> mc);
   VT0.rebuilder_simpleAssign_mcode = (fun mc -> mc);
   VT0.rebuilder_opAssign_mcode = (fun mc -> mc);
   VT0.rebuilder_fix_mcode = (fun mc -> mc);
   VT0.rebuilder_unary_mcode = (fun mc -> mc);
   VT0.rebuilder_arithOp_mcode = (fun mc -> mc);
   VT0.rebuilder_logicalOp_mcode = (fun mc -> mc);
   VT0.rebuilder_cv_mcode = (fun mc -> mc);
   VT0.rebuilder_sign_mcode = (fun mc -> mc);
   VT0.rebuilder_struct_mcode = (fun mc -> mc);
   VT0.rebuilder_storage_mcode = (fun mc -> mc);
   VT0.rebuilder_inc_mcode = (fun mc -> mc);
   VT0.rebuilder_dotsexprfn = (fun r k e -> k e);
   VT0.rebuilder_dotsinitfn = (fun r k e -> k e);
   VT0.rebuilder_dotsparamfn = (fun r k e -> k e);
   VT0.rebuilder_dotsstmtfn = (fun r k e -> k e);
   VT0.rebuilder_dotsdeclfn = (fun r k e -> k e);
   VT0.rebuilder_dotsfieldfn = (fun r k e -> k e);
   VT0.rebuilder_dotsenumdeclfn = (fun r k e -> k e);
   VT0.rebuilder_dotscasefn = (fun r k e -> k e);
   VT0.rebuilder_dotsdefparfn = (fun r k e -> k e);
   VT0.rebuilder_identfn = (fun r k e -> k e);
   VT0.rebuilder_exprfn = (fun r k e -> k e);
   VT0.rebuilder_assignOpfn = (fun r k e -> k e);
   VT0.rebuilder_binaryOpfn = (fun r k e -> k e);
   VT0.rebuilder_tyfn = (fun r k e -> k e);
   VT0.rebuilder_initfn = (fun r k e -> k e);
   VT0.rebuilder_paramfn = (fun r k e -> k e);
   VT0.rebuilder_declfn = (fun r k e -> k e);
   VT0.rebuilder_fieldfn = (fun r k e -> k e);
   VT0.rebuilder_enumdeclfn = (fun r k e -> k e);
   VT0.rebuilder_stmtfn = (fun r k e -> k e);
   VT0.rebuilder_forinfofn = (fun r k e -> k e);
   VT0.rebuilder_casefn = (fun r k e -> k e);
   VT0.rebuilder_string_fragmentfn = (fun r k e -> k e);
   VT0.rebuilder_attributefn = (fun r k e -> k e);
   VT0.rebuilder_attr_argfn = (fun r k e -> k e);
   VT0.rebuilder_topfn = (fun r k e -> k e)}

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
      VT0.rebuilder_rec_parameter_list =
      (function e -> let (_,e) = r.VT0.parameter_list e in e);
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

let rebuilder functions =
  let dz = rebuilder_dz in
  let xk k e = let (_,e) = k e in e in
  rebuilder_dz
    (visitor REBUILDER (fun x y -> x) ()
    (function mc -> ((),functions.VT0.rebuilder_meta_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_string_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_const_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_simpleAssign_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_opAssign_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_fix_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_unary_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_arithOp_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_logicalOp_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_cv_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_sign_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_struct_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_storage_mcode mc))
    (function mc -> ((),functions.VT0.rebuilder_inc_mcode mc))
    (fun r k e -> ((),functions.VT0.rebuilder_dotsexprfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_dotsinitfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_dotsparamfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_dotsstmtfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_dotsdeclfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_dotsfieldfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_dotsenumdeclfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_dotscasefn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_dotsdefparfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_identfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_exprfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_assignOpfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_binaryOpfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_tyfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_initfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_paramfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_declfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_fieldfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_enumdeclfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_stmtfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_forinfofn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_casefn (dz r) (xk k) e))
    (fun r k e ->
      ((),functions.VT0.rebuilder_string_fragmentfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_attributefn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_attr_argfn (dz r) (xk k) e))
    (fun r k e -> ((),functions.VT0.rebuilder_topfn (dz r) (xk k) e)))

let flat_rebuilder
    meta_mcode string_mcode const_mcode simpleAssign_mcode opAssign_mcode
    fix_mcode unary_mcode
    arithOp_mcode logicalOp_mcode cv_mcode sign_mcode struct_mcode
    storage_mcode inc_mcode
    dotsexprfn dotsinitfn dotsparamfn dotsstmtfn dotsdeclfn dotsfieldfn
    dotsenumdeclfn dotscasefn dotsdefparfn
    identfn exprfn assignOpfn arithOpfn tyfn initfn paramfn declfn fieldfn
    enumdeclfn
    stmtfn forinfofn casefn string_fragmentfn attributefn attr_argfn topfn =
  let dz = rebuilder_dz in
  let xk k e = let (_,e) = k e in e in
  rebuilder_dz
    (visitor REBUILDER (fun x y -> x) ()
    (function mc -> ((),meta_mcode mc))
    (function mc -> ((),string_mcode mc))
    (function mc -> ((),const_mcode mc))
    (function mc -> ((),simpleAssign_mcode mc))
    (function mc -> ((),opAssign_mcode mc))
    (function mc -> ((),fix_mcode mc))
    (function mc -> ((),unary_mcode mc))
    (function mc -> ((),arithOp_mcode mc))
    (function mc -> ((),logicalOp_mcode mc))
    (function mc -> ((),cv_mcode mc))
    (function mc -> ((),sign_mcode mc))
    (function mc -> ((),struct_mcode mc))
    (function mc -> ((),storage_mcode mc))
    (function mc -> ((),inc_mcode mc))
    (fun r k e -> ((),dotsexprfn (dz r) (xk k) e))
    (fun r k e -> ((),dotsinitfn (dz r) (xk k) e))
    (fun r k e -> ((),dotsparamfn (dz r) (xk k) e))
    (fun r k e -> ((),dotsstmtfn (dz r) (xk k) e))
    (fun r k e -> ((),dotsdeclfn (dz r) (xk k) e))
    (fun r k e -> ((),dotsfieldfn (dz r) (xk k) e))
    (fun r k e -> ((),dotsenumdeclfn (dz r) (xk k) e))
    (fun r k e -> ((),dotscasefn (dz r) (xk k) e))
    (fun r k e -> ((),dotsdefparfn (dz r) (xk k) e))
    (fun r k e -> ((),identfn (dz r) (xk k) e))
    (fun r k e -> ((),exprfn (dz r) (xk k) e))
    (fun r k e -> ((),assignOpfn (dz r) (xk k) e))
    (fun r k e -> ((),arithOpfn (dz r) (xk k) e))
    (fun r k e -> ((),tyfn (dz r) (xk k) e))
    (fun r k e -> ((),initfn (dz r) (xk k) e))
    (fun r k e -> ((),paramfn (dz r) (xk k) e))
    (fun r k e -> ((),declfn (dz r) (xk k) e))
    (fun r k e -> ((),fieldfn (dz r) (xk k) e))
    (fun r k e -> ((),enumdeclfn (dz r) (xk k) e))
    (fun r k e -> ((),stmtfn (dz r) (xk k) e))
    (fun r k e -> ((),forinfofn (dz r) (xk k) e))
    (fun r k e -> ((),casefn (dz r) (xk k) e))
    (fun r k e -> ((),string_fragmentfn (dz r) (xk k) e))
    (fun r k e -> ((),attributefn (dz r) (xk k) e))
    (fun r k e -> ((),attr_argfn (dz r) (xk k) e))
    (fun r k e -> ((),topfn (dz r) (xk k) e)))

let combiner_rebuilder_functions =
  {VT0.combiner_rebuilder_meta_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_string_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_const_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_simpleAssign_mcode =
     (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_opAssign_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_fix_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_unary_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_arithOp_mcode =
     (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_logicalOp_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_cv_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_sign_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_struct_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_storage_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_inc_mcode =
    (fun opt_default mc -> (opt_default,mc));
   VT0.combiner_rebuilder_dotsexprfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_dotsinitfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_dotsparamfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_dotsstmtfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_dotsdeclfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_dotsfieldfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_dotsenumdeclfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_dotscasefn = (fun r k e -> k e);
   VT0.combiner_rebuilder_dotsdefparfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_identfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_exprfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_assignOpfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_binaryOpfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_tyfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_initfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_paramfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_declfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_fieldfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_enumdeclfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_stmtfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_forinfofn = (fun r k e -> k e);
   VT0.combiner_rebuilder_casefn = (fun r k e -> k e);
   VT0.combiner_rebuilder_string_fragmentfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_attributefn = (fun r k e -> k e);
   VT0.combiner_rebuilder_attr_argfn = (fun r k e -> k e);
   VT0.combiner_rebuilder_topfn = (fun r k e -> k e)}

let combiner_rebuilder bind option_default functions =
  visitor BOTH bind option_default
    (functions.VT0.combiner_rebuilder_meta_mcode option_default)
    (functions.VT0.combiner_rebuilder_string_mcode option_default)
    (functions.VT0.combiner_rebuilder_const_mcode option_default)
    (functions.VT0.combiner_rebuilder_simpleAssign_mcode option_default)
    (functions.VT0.combiner_rebuilder_opAssign_mcode option_default)
    (functions.VT0.combiner_rebuilder_fix_mcode option_default)
    (functions.VT0.combiner_rebuilder_unary_mcode option_default)
    (functions.VT0.combiner_rebuilder_arithOp_mcode option_default)
    (functions.VT0.combiner_rebuilder_logicalOp_mcode option_default)
    (functions.VT0.combiner_rebuilder_cv_mcode option_default)
    (functions.VT0.combiner_rebuilder_sign_mcode option_default)
    (functions.VT0.combiner_rebuilder_struct_mcode option_default)
    (functions.VT0.combiner_rebuilder_storage_mcode option_default)
    (functions.VT0.combiner_rebuilder_inc_mcode option_default)
    functions.VT0.combiner_rebuilder_dotsexprfn
    functions.VT0.combiner_rebuilder_dotsinitfn
    functions.VT0.combiner_rebuilder_dotsparamfn
    functions.VT0.combiner_rebuilder_dotsstmtfn
    functions.VT0.combiner_rebuilder_dotsdeclfn
    functions.VT0.combiner_rebuilder_dotsfieldfn
    functions.VT0.combiner_rebuilder_dotsenumdeclfn
    functions.VT0.combiner_rebuilder_dotscasefn
    functions.VT0.combiner_rebuilder_dotsdefparfn
    functions.VT0.combiner_rebuilder_identfn
    functions.VT0.combiner_rebuilder_exprfn
    functions.VT0.combiner_rebuilder_assignOpfn
    functions.VT0.combiner_rebuilder_binaryOpfn
    functions.VT0.combiner_rebuilder_tyfn
    functions.VT0.combiner_rebuilder_initfn
    functions.VT0.combiner_rebuilder_paramfn
    functions.VT0.combiner_rebuilder_declfn
    functions.VT0.combiner_rebuilder_fieldfn
    functions.VT0.combiner_rebuilder_enumdeclfn
    functions.VT0.combiner_rebuilder_stmtfn
    functions.VT0.combiner_rebuilder_forinfofn
    functions.VT0.combiner_rebuilder_casefn
    functions.VT0.combiner_rebuilder_string_fragmentfn
    functions.VT0.combiner_rebuilder_attributefn
    functions.VT0.combiner_rebuilder_attr_argfn
    functions.VT0.combiner_rebuilder_topfn
