(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)
(* Generic traversal: combiner *)
(* parameters:
   combining function
   treatment of: mcode, identifiers, expressions, fullTypes, types,
   declarations, statements, toplevels
   default value for options *)

type 'a combiner =
    {combiner_ident : Ast.ident -> 'a;
     combiner_expression : Ast.expression -> 'a;
     combiner_fragment : Ast.string_fragment -> 'a;
     combiner_format : Ast.string_format -> 'a;
     combiner_assignOp : Ast_cocci.assignOp -> 'a;
     combiner_binaryOp : Ast_cocci.binaryOp -> 'a;
     combiner_fullType : Ast.fullType -> 'a;
     combiner_typeC : Ast.typeC -> 'a;
     combiner_declaration : Ast.declaration -> 'a;
     combiner_field : Ast.field -> 'a;
     combiner_ann_field : Ast.annotated_field -> 'a;
     combiner_enumdecl : Ast_cocci.enum_decl -> 'a;
     combiner_initialiser : Ast.initialiser -> 'a;
     combiner_parameter : Ast.parameterTypeDef -> 'a;
     combiner_parameter_list : Ast.parameter_list -> 'a;
     combiner_rule_elem : Ast.rule_elem -> 'a;
     combiner_statement : Ast.statement -> 'a;
     combiner_case_line : Ast.case_line -> 'a;
     combiner_attribute : Ast.attr -> 'a;
     combiner_attr_arg : Ast.attr_arg -> 'a;
     combiner_top_level : Ast.top_level -> 'a;
     combiner_anything : Ast.anything  -> 'a;
     combiner_expression_dots : Ast.expression Ast.dots -> 'a;
     combiner_statement_dots : Ast.statement Ast.dots -> 'a;
     combiner_anndecl_dots : Ast.annotated_decl Ast.dots -> 'a;
     combiner_annfield_dots : Ast.annotated_field Ast.dots -> 'a;
     combiner_enumdecl_dots : Ast.enum_decl Ast.dots -> 'a;
     combiner_initialiser_dots : Ast.initialiser Ast.dots -> 'a}

type ('mc,'a) cmcode = 'a combiner -> 'mc Ast_cocci.mcode -> 'a
type ('cd,'a) ccode = 'a combiner -> ('cd -> 'a) -> 'cd -> 'a

let combiner bind option_default
    meta_mcodefn string_mcodefn const_mcodefn simpleassign_mcodefn
    opassign_mcodefn fix_mcodefn
    unary_mcodefn arithop_mcodefn logicalop_mcodefn
    cv_mcodefn sign_mcodefn struct_mcodefn storage_mcodefn
    inc_file_mcodefn
    expdotsfn paramdotsfn stmtdotsfn anndecldotsfn annfielddotsfn
    enumdecldotsfn initdotsfn
    identfn exprfn fragfn fmtfn assignOpfn binaryOpfn ftfn tyfn initfn
    paramfn define_paramfn declfn
    annotated_declfn fieldfn annotated_fieldfn enum_declfn rulefn stmtfn
    casefn attributefn attr_argfn topfn anyfn =
  let multibind l =
    let rec loop = function
	[] -> option_default
      |	[x] -> x
      |	x::xs -> bind x (loop xs) in
    loop l in
  let get_option f = function
      Some x -> f x
    | None -> option_default in

  let dotsfn param default all_functions arg =
    let k d = multibind (List.map default (Ast.unwrap d)) in
    param all_functions k arg in

  let rec meta_mcode x = meta_mcodefn all_functions x
  and string_mcode x = string_mcodefn all_functions x
  and const_mcode x = const_mcodefn all_functions x
  and simpleassign_mcode x = simpleassign_mcodefn all_functions x
  and opassign_mcode x = opassign_mcodefn all_functions x
  and fix_mcode x = fix_mcodefn all_functions x
  and unary_mcode x = unary_mcodefn all_functions x
  and arithop_mcode x = arithop_mcodefn all_functions x
  and logicalop_mcode x = logicalop_mcodefn all_functions x
  and cv_mcode x = cv_mcodefn all_functions x
  and sign_mcode x = sign_mcodefn all_functions x
  and struct_mcode x = struct_mcodefn all_functions x
  and storage_mcode x = storage_mcodefn all_functions x
  and inc_file_mcode x = inc_file_mcodefn all_functions x

  and strdotsfn all_functions k arg = k arg
  and ecdotsfn all_functions k arg = k arg
  and defpardotsfn all_functions k arg = k arg

  and expression_dots d = dotsfn expdotsfn expression all_functions d
  and parameter_dots d = dotsfn paramdotsfn parameterTypeDef all_functions d
  and statement_dots d = dotsfn stmtdotsfn statement all_functions d
  and annotated_decl_dots d =
    dotsfn anndecldotsfn annotated_decl all_functions d
  and annotated_field_dots d =
    dotsfn annfielddotsfn annotated_field all_functions d
  and enum_decl_dots d =
    dotsfn enumdecldotsfn enum_decl all_functions d
  and initialiser_dots d = dotsfn initdotsfn initialiser all_functions d
  and string_fragment_dots d = dotsfn strdotsfn string_fragment all_functions d
  and exec_code_dots d = dotsfn ecdotsfn exec_code all_functions d
  and define_param_dots d = dotsfn defpardotsfn define_param all_functions d

  and ident i =
    let k i =
      match Ast.unwrap i with
	Ast.Id(name) -> string_mcode name
      | Ast.MetaId(name,_,_,_) -> meta_mcode name
      | Ast.MetaFunc(name,_,_,_) -> meta_mcode name
      | Ast.MetaLocalFunc(name,_,_,_) -> meta_mcode name
      | Ast.AsIdent(id,asid) ->
(* use let-ins to ensure arg evaluation happens left-to-right *)
	  let lid = ident id in
	  let lasid = ident asid in
	  bind lid lasid
      | Ast.DisjId(id_list) | Ast.ConjId(id_list) ->
	  multibind (List.map ident id_list)
      | Ast.OptIdent(id) -> ident id in
    identfn all_functions k i

  and expression e =
    let k e =
      match Ast.unwrap e with
	Ast.Ident(id) -> ident id
      | Ast.Constant(const) -> const_mcode const
      | Ast.StringConstant(lq,str,rq,_sz) ->
	  let llq = string_mcode lq in
	  let lstr = string_fragment_dots str in
	  let lrq = string_mcode rq in
	  multibind [llq; lstr; lrq]
      | Ast.FunCall(fn,lp,args,rp) ->
	  let lfn = expression fn in
	  let llp = string_mcode lp in
	  let largs = expression_dots args in
	  let lrp = string_mcode rp in
	  multibind [lfn; llp; largs; lrp]
      | Ast.Assignment(left,op,right,simple) ->
	  let lleft = expression left in
	  let lop = assignOp op in
	  let lright = expression right in
	  multibind [lleft; lop; lright]
      | Ast.Sequence(left,op,right) ->
	  let lleft = expression left in
	  let lop = string_mcode op in
	  let lright = expression right in
	  multibind [lleft; lop; lright]
      | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
	  let lexp1 = expression exp1 in
	  let lwhy = string_mcode why in
	  let lexp2 = get_option expression exp2 in
	  let lcolon = string_mcode colon in
	  let lexp3 = expression exp3 in
	  multibind [lexp1; lwhy; lexp2; lcolon; lexp3]
      | Ast.Postfix(exp,op) ->
	  let lexp = expression exp in
	  let lop = fix_mcode op in
	  bind lexp lop
      | Ast.Infix(exp,op) ->
	  let lop = fix_mcode op in
	  let lexp = expression exp in
	  bind lop lexp
      | Ast.Unary(exp,op) ->
	  let lop = unary_mcode op in
	  let lexp = expression exp in
	  bind lop lexp
      | Ast.Binary(left,op,right) ->
	  let lleft = expression left in
	  let lop = binaryOp op in
	  let lright = expression right in
	  multibind [lleft; lop; lright]
      | Ast.Nested(left,op,right) ->
	  let lleft = expression left in
	  let lop = binaryOp op in
	  let lright = expression right in
	  multibind [lleft; lop; lright]
      | Ast.Paren(lp,exp,rp) ->
	  let llp = string_mcode lp in
	  let lexp = expression exp in
	  let lrp = string_mcode rp in
	  multibind [llp; lexp; lrp]
      | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
	  let lexp1 = expression exp1 in
	  let llb = string_mcode lb in
	  let lexp2 = expression exp2 in
	  let lrb = string_mcode rb in
	  multibind [lexp1; llb; lexp2; lrb]
      | Ast.RecordAccess(exp,pt,field) ->
	  let lexp = expression exp in
	  let lpt = string_mcode pt in
	  let lfield = ident field in
	  multibind [lexp; lpt; lfield]
      | Ast.RecordPtAccess(exp,ar,field) ->
	  let lexp = expression exp in
	  let lar = string_mcode ar in
	  let lfield = ident field in
	  multibind [lexp; lar; lfield]
      | Ast.Cast(lp,ty,attr,rp,exp) ->
	  let llp = string_mcode lp in
	  let lty = fullType ty in
	  let lattr = multibind (List.map attribute attr) in
	  let lrp = string_mcode rp in
	  let lexp = expression exp in
          multibind [llp; lty; lattr; lrp; lexp]
      | Ast.SizeOfExpr(szf,exp) ->
	  let lszf = string_mcode szf in
	  let lexp = expression exp in
	  bind lszf lexp
      | Ast.SizeOfType(szf,lp,ty,rp) ->
	  let lszf = string_mcode szf in
	  let llp = string_mcode lp in
	  let lty = fullType ty in
	  let lrp = string_mcode rp in
	  multibind [lszf; llp; lty; lrp]
      | Ast.TypeExp(ty) -> fullType ty
      | Ast.Constructor(lp,ty,rp,init) ->
	  let llp = string_mcode lp in
	  let lty = fullType ty in
	  let lrp = string_mcode rp in
	  let linit = initialiser init in
	  multibind [llp; lty; lrp; linit]
      | Ast.MetaErr(name,_,_,_)
      | Ast.MetaExpr(name,_,_,_,_,_,_)
      | Ast.MetaExprList(name,_,_,_,_) -> meta_mcode name
      |	Ast.AsExpr(exp,asexp) ->
	  let lexp = expression exp in
	  let lasexp = expression asexp in
	  bind lexp lasexp
      | Ast.AsSExpr(exp,asstm) -> 
	  let lexp = expression exp in
	  let lasstm = rule_elem asstm in
	  bind lexp lasstm
      | Ast.EComma(cm) -> string_mcode cm
      | Ast.DisjExpr(exp_list) | Ast.ConjExpr(exp_list) ->
	  multibind (List.map expression exp_list)
      | Ast.NestExpr(starter,expr_dots,ender,whncode,multi) ->
	  let lstarter = string_mcode starter in
	  let lexpr_dots = expression_dots expr_dots in
	  let lender = string_mcode ender in
	  let lwhncode = get_option expression whncode in
	  multibind [lstarter; lexpr_dots; lender; lwhncode]
      | Ast.Edots(dots,whncode) ->
	  let ldots = string_mcode dots in
	  let lwhncode = get_option expression whncode in
	  bind ldots lwhncode
      | Ast.OptExp(exp) -> expression exp in
    exprfn all_functions k e

  and assignOp op =
    let k e =
      match Ast.unwrap e with
        Ast.SimpleAssign o -> simpleassign_mcode o
      | Ast.OpAssign o -> opassign_mcode o
      | Ast.MetaAssign (mv,_,_,_) -> meta_mcode mv in
    assignOpfn all_functions k op

  and binaryOp op =
    let k e =
      match Ast.unwrap e with
        Ast.Arith o -> arithop_mcode o
      | Ast.Logical o -> logicalop_mcode o
      | Ast.MetaBinary (mv,_,_,_) -> meta_mcode mv in
    binaryOpfn all_functions k op

  and string_fragment e =
    let k e =
      match Ast.unwrap e with
	Ast.ConstantFragment(str) -> string_mcode str
      | Ast.FormatFragment(pct,fmt) ->
	  let pct = string_mcode pct in
	  let fmt = string_format fmt in
	  bind pct fmt
      |	Ast.Strdots dots -> string_mcode dots
      | Ast.MetaFormatList(pct,name,lenname,_,_,_) ->
	  let pct = string_mcode pct in
	  let name = meta_mcode name in
	  bind pct name in
    fragfn all_functions k e

  and string_format e =
    let k e =
      match Ast.unwrap e with
	Ast.ConstantFormat(str) -> string_mcode str
      | Ast.MetaFormat(name,_,_,_) -> meta_mcode name in
    fmtfn all_functions k e

  and fullType ft =
    let k ft =
      match Ast.unwrap ft with
	Ast.Type(_,cv,ty) ->
	  let lcv = get_option cv_mcode cv in
	  let lty = typeC ty in
	  bind lcv lty
      |	Ast.AsType(ty,asty) ->
	  let lty = fullType ty in
	  let lasty = fullType asty in
	  bind lty lasty
      | Ast.DisjType(types) | Ast.ConjType(types) ->
	  multibind (List.map fullType types)
      | Ast.OptType(ty) -> fullType ty in
    ftfn all_functions k ft

  and array_type (ty,(id : Ast.ident option),lb,size,rb) =
    let lty = fullType ty in
    let lid = match id with Some idd -> [ident idd] | None -> [] in
    let lb = string_mcode lb in
    let lsize = get_option expression size in
    let lrb = string_mcode rb in
    multibind ([lty] @ lid @ [lb; lsize; lrb])

  and parentype_type (lp,ty,(id : Ast.ident option),rp) =
    let function_pointer ty1 array_dec =
      match Ast.unwrap ty1 with
       Ast.Type(_,_,fty1) ->
        (match Ast.unwrap fty1 with
          Ast.Pointer(ty2,star) ->
           (match Ast.unwrap ty2 with
             Ast.Type(_,_,fty3) ->
              (match Ast.unwrap fty3 with
                Ast.FunctionType(ty3,lp3,params,rp3) ->
                 let ltyp = fullType ty3 in
                 let llp = string_mcode lp in
                 let lstar = string_mcode star in
                 let lid =
                   match id with
                     Some idd -> [ident idd]
                   | None -> [] in
                 let larray =
                   match array_dec with
                     Some(lb1,size1,rb1) ->
                       let llb1 = string_mcode lb1 in
                       let lsize1 = get_option expression size1 in
                       let lrb1 = string_mcode rb1 in
                       [llb1;lsize1;lrb1]
                   | None -> [] in
                 let lrp = string_mcode rp in
                 let llp3 = string_mcode lp3 in
                 let lparams = parameter_dots params in
                 let lrp3 = string_mcode rp3 in
                 multibind
                   ([ltyp;llp;lstar] @ lid @ larray @
                    [lrp;llp3;lparams;lrp3])
             | _ -> failwith "ParenType Visitor_ast")
           | _ -> failwith "ParenType Visitor_ast")
         | _ -> failwith "ParenType Visitor_ast")
       | _ -> failwith "ParenType Visitor_ast" in
    match Ast.unwrap ty with
      Ast.Type(_,_,fty1) ->
        (match Ast.unwrap fty1 with
          Ast.Array(ty1,lb1,size,rb1) ->
            function_pointer ty1 (Some(lb1,size,rb1))
        | Ast.Pointer(ty1,star) ->
            function_pointer ty None
        | _ -> failwith "ParenType Visitor_ast")
     | _ -> failwith "ParenType Visitor_ast"

  and typeC ty =
    let k ty =
      match Ast.unwrap ty with
	Ast.BaseType(ty,strings) -> multibind (List.map string_mcode strings)
      | Ast.SignedT(sgn,ty) ->
	  let lsgn = sign_mcode sgn in
	  let lty = get_option typeC ty in
	  bind lsgn lty
      | Ast.Pointer(ty,star) ->
	  let lty = fullType ty in
	  let lstar = string_mcode star in
	  bind lty lstar
      | Ast.ParenType(lp,ty,rp) ->
          parentype_type (lp,ty,None,rp)
      | Ast.FunctionType(ty,lp,params,rp) ->
          let lty = fullType ty in
          let llp = string_mcode lp in
          let lparams = parameter_dots params in
          let lrp = string_mcode rp in
          multibind [lty; llp; lparams; lrp]
      | Ast.Array(ty,lb,size,rb) -> array_type (ty,None,lb,size,rb)
      | Ast.Decimal(dec,lp,length,comma,precision_opt,rp) ->
	  let ldec = string_mcode dec in
	  let llp = string_mcode lp in
	  let llength = expression length in
	  let lcomma = get_option string_mcode comma in
	  let lprecision_opt = get_option expression precision_opt in
	  let lrp = string_mcode rp in
	  multibind [ldec; llp; llength; lcomma; lprecision_opt; lrp]
      | Ast.EnumName(kind,name) ->
	  let lkind = string_mcode kind in
	  let lname = get_option ident name in
	  bind lkind lname
      | Ast.EnumDef(ty,lb,ids,rb) ->
	  let lty = fullType ty in
	  let llb = string_mcode lb in
	  let lids = enum_decl_dots ids in
	  let lrb = string_mcode rb in
	  multibind [lty; llb; lids; lrb]
      | Ast.StructUnionName(kind,name) ->
	  let lkind = struct_mcode kind in
	  let lname = get_option ident name in
	  bind lkind lname
      | Ast.StructUnionDef(ty,lb,decls,rb) ->
	  let lty = fullType ty in
	  let llb = string_mcode lb in
	  let ldecls = annotated_field_dots decls in
	  let lrb = string_mcode rb in
	  multibind [lty; llb; ldecls; lrb]
      | Ast.TypeOfExpr(tf,lp,e,rp) ->
	  let ltf = string_mcode tf in
	  let llp = string_mcode lp in
	  let le = expression e in
	  let lrp = string_mcode rp in
	  multibind [ltf; llp; le; lrp]
      | Ast.TypeOfType(tf,lp,ty,rp) ->
	  let ltf = string_mcode tf in
	  let llp = string_mcode lp in
	  let lty = fullType ty in
	  let lrp = string_mcode rp in
	  multibind [ltf; llp; lty; lrp]
      | Ast.TypeName(name) -> string_mcode name
      | Ast.AutoType(auto) -> string_mcode auto
      | Ast.MetaType(name,_,_,_) -> meta_mcode name in
    tyfn all_functions k ty

  and named_type ty id =
    match Ast.unwrap ty with
      Ast.Type(_,None,ty1) ->
	(match Ast.unwrap ty1 with
	| Ast.Array(ty,lb,size,rb) -> array_type (ty, Some id, lb, size, rb)
        | Ast.ParenType(lp,ty,rp) -> parentype_type (lp, ty, Some id, rp)
	| _ -> let lty = fullType ty in
	       let lid = ident id in
	       bind lty lid)
    | _ -> let lty = fullType ty in
	   let lid = ident id in
	   bind lty lid

  and declaration d =
    let k d =
      match Ast.unwrap d with
	Ast.MetaDecl(name,_,_,_) ->
	  meta_mcode name
      |	Ast.AsDecl(decl,asdecl) ->
	  let ldecl = declaration decl in
	  let lasdecl = declaration asdecl in
	  bind ldecl lasdecl
      |	Ast.Init(stg,ty,id,attr,eq,ini,sem) ->
	  let lstg = get_option storage_mcode stg in
	  let lid = named_type ty id in
	  let lattr = multibind (List.map attribute attr) in
	  let leq = string_mcode eq in
	  let lini = initialiser ini in
	  let lsem = string_mcode sem in
	  multibind [lstg; lid; lattr; leq; lini; lsem]
      | Ast.UnInit(stg,ty,id,attr,sem) ->
	  let lstg = get_option storage_mcode stg in
	  let lid = named_type ty id in
	  let lattr = multibind (List.map attribute attr) in
	  let lsem = string_mcode sem in
	  multibind [lstg; lid; lattr; lsem]
      | Ast.FunProto(fi,name,lp1,params,va,rp1,sem) ->
	  let lfi = List.map fninfo fi in
	  let lname = ident name in
	  let llp1 = string_mcode lp1 in
	  let lparams = parameter_dots params in
          let (lcomma,lellipsis) = match va with
            | None -> ([],[])
            | Some (comma,ellipsis) ->
		([string_mcode comma],[string_mcode ellipsis]) in
	  let lrp1 = string_mcode rp1 in
	  multibind
	    (lfi @ [lname; llp1; lparams] @ lcomma @ lellipsis @ [lrp1])
      | Ast.MacroDecl(stg,name,lp,args,rp,attr,sem) ->
	  let lstg = get_option storage_mcode stg in
	  let lname = ident name in
	  let llp = string_mcode lp in
	  let largs = expression_dots args in
	  let lrp = string_mcode rp in
	  let lattr = multibind (List.map attribute attr) in
	  let lsem = string_mcode sem in
	  multibind [lstg; lname; llp; largs; lrp; lattr; lsem]
      | Ast.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) ->
	  let lstg = get_option storage_mcode stg in
	  let lname = ident name in
	  let llp = string_mcode lp in
	  let largs = expression_dots args in
	  let lrp = string_mcode rp in
	  let leq = string_mcode eq in
	  let lini = initialiser ini in
	  let lsem = string_mcode sem in
	  multibind [lstg; lname; llp; largs; lrp; leq; lini; lsem]
      | Ast.TyDecl(ty,attr,sem) ->
	  let lty = fullType ty in
	  let lattr = multibind (List.map attribute attr) in
	  let lsem = string_mcode sem in
	  multibind [lty; lattr; lsem]
      | Ast.Typedef(stg,ty,id,sem) ->
	  let lstg = string_mcode stg in
	  let lty = fullType ty in
	  let lid = typeC id in
	  let lsem = string_mcode sem in
	  multibind [lstg; lty; lid; lsem]
      | Ast.DisjDecl(decls)
      | Ast.ConjDecl(decls) -> multibind (List.map declaration decls)
      | Ast.OptDecl(decl) -> declaration decl in
    declfn all_functions k d

  and annotated_decl d =
    let k d =
      match Ast.unwrap d with
	Ast.DElem(_,_,d) -> declaration d in
    annotated_declfn all_functions k d

  and field d =
    let k d =
      match Ast.unwrap d with
	Ast.MetaField(name,_,_,_) | Ast.MetaFieldList(name,_,_,_,_) ->
	  meta_mcode name
      | Ast.Field(ty,id,bf,sem) ->
	  let lid =
	    match id with
	      None -> fullType ty
	    | Some id -> named_type ty id in
	  let bitfield (c, e) =
	    let lc = string_mcode c in
	    let le = expression e in
	    [lc; le] in
	  let lbf = Common.default [] bitfield bf in
	  let lsem = string_mcode sem in
	  multibind ([lid] @ lbf @ [lsem]) in
    fieldfn all_functions k d

  and annotated_field d =
    let k d =
      match Ast.unwrap d with
	Ast.FElem(_,_,d) -> field d
      | Ast.Fdots(dots,whncode) ->
	  let ldots = string_mcode dots in
	  let lwhncode = get_option field whncode in
	  bind ldots lwhncode
      | Ast.DisjField(decls)
      | Ast.ConjField(decls) ->
	  multibind (List.map annotated_field decls)
      | Ast.OptField(decl) -> annotated_field decl in
    annotated_fieldfn all_functions k d

  and enum_decl d =
    let k d =
      match Ast.unwrap d with
	Ast.Enum(name,enum_val) ->
          let lname = ident name in
          (match enum_val with
            None -> lname
          | Some(eq,eval) ->
              let leq = string_mcode eq in
              let leval = expression eval in
              multibind [lname; leq; leval])
      | Ast.EnumComma(cm) ->
        string_mcode cm
      | Ast.EnumDots(dots,whncode) ->
        let ldots = string_mcode dots in
        let lwhncode = get_option enum_decl whncode in
	bind ldots lwhncode in
    enum_declfn all_functions k d

  and initialiser i =
    let k i =
      match Ast.unwrap i with
	Ast.MetaInit(name,_,_,_) -> meta_mcode name
      |	Ast.MetaInitList(name,_,_,_,_) -> meta_mcode name
      |	Ast.AsInit(init,asinit) ->
	  let linit = initialiser init in
	  let lasinit = initialiser asinit in
	  bind linit lasinit
      |	Ast.InitExpr(exp) -> expression exp
      | Ast.ArInitList(lb,initlist,rb) ->
	  let llb = string_mcode lb in
	  let linitlist = initialiser_dots initlist in
	  let lrb = string_mcode rb in
	  multibind [llb; linitlist; lrb]
      | Ast.StrInitList(allminus,lb,initlist,rb,whncode) ->
	  let llb = string_mcode lb in
	  let linitlist = multibind (List.map initialiser initlist) in
	  let lrb = string_mcode rb in
	  let lwhncode = multibind (List.map initialiser whncode) in
	  multibind [llb; linitlist; lrb; lwhncode]
      | Ast.InitGccName(name,eq,ini) ->
	  let lname = ident name in
	  let leq = string_mcode eq in
	  let lini = initialiser ini in
	  multibind [lname; leq; lini]
      | Ast.InitGccExt(designators,eq,ini) ->
	  let ldesignators = List.map designator designators in
	  let leq = string_mcode eq in
	  let lini = initialiser ini in
	  multibind (ldesignators @ [leq; lini])
      | Ast.IComma(cm) -> string_mcode cm
      | Ast.Idots(dots,whncode) ->
	  let ldots = string_mcode dots in
	  let lwhncode = get_option initialiser whncode in
	  bind ldots lwhncode
      | Ast.OptIni(i) -> initialiser i in
    initfn all_functions k i

  and designator = function
      Ast.DesignatorField(dot,id) ->
	let ldot = string_mcode dot in
	let lid = ident id in
	bind ldot lid
    | Ast.DesignatorIndex(lb,exp,rb) ->
	let llb = string_mcode lb in
	let lexp = expression exp in
	let lrb = string_mcode rb in
	multibind [llb; lexp; lrb]
    | Ast.DesignatorRange(lb,min,dots,max,rb) ->
	let llb = string_mcode lb in
	let lmin = expression min in
	let ldots = string_mcode dots in
	let lmax = expression max in
	let lrb = string_mcode rb in
	multibind [llb; lmin; ldots; lmax; lrb]

  and parameterTypeDef p =
    let k p =
      match Ast.unwrap p with
        Ast.VoidParam(ty,attr) ->
          let lty = fullType ty in
          let lattr = multibind (List.map attribute attr) in
          bind lty lattr
      | Ast.Param(ty,Some id,attr) ->
          let lid = named_type ty id in
          let lattr = multibind (List.map attribute attr) in
          bind lid lattr
      | Ast.Param(ty,None,attr) ->
          let lty = fullType ty in
          let lattr = multibind (List.map attribute attr) in
          bind lty lattr
      | Ast.MetaParam(name,_,_,_) -> meta_mcode name
      | Ast.MetaParamList(name,_,_,_,_) -> meta_mcode name
      |	Ast.AsParam(p,asexp) ->
	  let lp = parameterTypeDef p in
	  let lasexp = expression asexp in
	  bind lp lasexp
      | Ast.PComma(cm) -> string_mcode cm
      | Ast.Pdots(dots) -> string_mcode dots
      | Ast.OptParam(param) -> parameterTypeDef param in
    paramfn all_functions k p

  and rule_elem re =
    let k re =
      match Ast.unwrap re with
	Ast.FunHeader(_,_,fi,name,lp,params,va,rp) ->
	  let lfi = List.map fninfo fi in
	  let lname = ident name in
	  let llp = string_mcode lp in
	  let lparams = parameter_dots params in
          let (lcomma,lellipsis) = match va with
            | None -> ([],[])
            | Some (comma,ellipsis) -> ([string_mcode comma],[string_mcode ellipsis]) in
	  let lrp = string_mcode rp in
	  multibind (lfi @ [lname; llp; lparams] @ lcomma @ lellipsis @ [lrp])
      | Ast.Decl decl -> annotated_decl decl
      | Ast.SeqStart(brace) -> string_mcode brace
      | Ast.SeqEnd(brace) -> string_mcode brace
      | Ast.ExprStatement(exp,sem) ->
	  let lexp = get_option expression exp in
	  let lsem = string_mcode sem in
	  bind lexp lsem
      | Ast.IfHeader(iff,lp,exp,rp) ->
	  let liff = string_mcode iff in
	  let llp = string_mcode lp in
	  let lexp = expression exp in
	  let lrp = string_mcode rp in
	  multibind [liff; llp; lexp; lrp]
      | Ast.Else(els) -> string_mcode els
      | Ast.WhileHeader(whl,lp,exp,rp) ->
	  let lwhl = string_mcode whl in
	  let llp = string_mcode lp in
	  let lexp = expression exp in
	  let lrp = string_mcode rp in
	  multibind [lwhl; llp; lexp; lrp]
      | Ast.DoHeader(d) -> string_mcode d
      | Ast.WhileTail(whl,lp,exp,rp,sem) ->
	  let lwhl = string_mcode whl in
	  let llp = string_mcode lp in
	  let lexp = expression exp in
	  let lrp = string_mcode rp in
	  let lsem = string_mcode sem in
	  multibind [lwhl; llp; lexp; lrp; lsem]
      | Ast.ForHeader(fr,lp,first,e2,sem2,e3,rp) ->
	  let lfr = string_mcode fr in
	  let llp = string_mcode lp in
	  let lfirst = forinfo first in
	  let le2 = get_option expression e2 in
	  let lsem2 = string_mcode sem2 in
	  let le3 = get_option expression e3 in
	  let lrp = string_mcode rp in
	  multibind [lfr; llp; lfirst; le2; lsem2; le3; lrp]
      | Ast.IteratorHeader(nm,lp,args,rp) ->
	  let lnm = ident nm in
	  let llp = string_mcode lp in
	  let largs = expression_dots args in
	  let lrp = string_mcode rp in
	  multibind [lnm; llp; largs; lrp]
      | Ast.SwitchHeader(switch,lp,exp,rp) ->
	  let lswitch = string_mcode switch in
	  let llp = string_mcode lp in
	  let lexp = expression exp in
	  let lrp = string_mcode rp in
	  multibind [lswitch; llp; lexp; lrp]
      | Ast.Break(br,sem) ->
	  let lbr = string_mcode br in
	  let lsem = string_mcode sem in
	  bind lbr lsem
      | Ast.Continue(cont,sem) ->
	  let lcont = string_mcode cont in
	  let lsem = string_mcode sem in
	  bind lcont lsem
      |	Ast.Label(l,dd) ->
	  let ll = ident l in
	  let ldd = string_mcode dd in
	  bind ll ldd
      |	Ast.Goto(goto,l,sem) ->
	  let lgoto = string_mcode goto in
	  let ll = ident l in
	  let lsem = string_mcode sem in
	  multibind [lgoto; ll; lsem]
      | Ast.Return(ret,sem) ->
	  let lret = string_mcode ret in
	  let lsem = string_mcode sem in
	  bind lret lsem
      | Ast.ReturnExpr(ret,exp,sem) ->
	  let lret = string_mcode ret in
	  let lexp = expression exp in
	  let lsem = string_mcode sem in
	  multibind [lret; lexp; lsem]
      |	Ast.Exec(exec,lang,code,sem) ->
	  let lexec = string_mcode exec in
	  let lland = string_mcode lang in
	  let lcode = exec_code_dots code in
	  let lsem = string_mcode sem in
	  multibind [lexec; lland; lcode; lsem]
      | Ast.MetaStmt(name,_,_,_,_) -> meta_mcode name
      | Ast.MetaStmtList(name,_,_,_,_) -> meta_mcode name
      | Ast.MetaRuleElem(name,_,_,_) -> meta_mcode name
      | Ast.Exp(exp) -> expression exp
      | Ast.TopExp(exp) -> expression exp
      | Ast.Ty(ty) -> fullType ty
      | Ast.TopId(ty) -> ident ty
      | Ast.TopInit(init) -> initialiser init
      |	Ast.Include(inc,name) ->
	  let linc = string_mcode inc in
	  let lname = inc_file_mcode name in
	  bind linc lname
      | Ast.MetaInclude(inc,name) ->
	  let linc = string_mcode inc in
	  let lname = expression name in
	  bind linc lname
      |	Ast.Undef(def,id) ->
	  let ldef = string_mcode def in
	  let lid = ident id in
	  bind ldef lid
      |	Ast.DefineHeader(def,id,params) ->
	  let ldef = string_mcode def in
	  let lid = ident id in
	  let lparams = define_parameters params in
	  multibind [ldef; lid; lparams]
      |	Ast.Pragma(prg,id,body) ->
	  let lprg = string_mcode prg in
	  let lid = ident id in
	  let lbody = pragmainfo body in
	  multibind [lprg; lid; lbody]
      |	Ast.Default(def,colon) ->
	  let ldef = string_mcode def in
	  let lcolon = string_mcode colon in
	  bind ldef lcolon
      |	Ast.Case(case,exp,colon) ->
	  let lcase = string_mcode case in
	  let lexp = expression exp in
	  let lcolon = string_mcode colon in
	  multibind [lcase; lexp; lcolon]
      | Ast.AsRe(re,asre) ->
	  let re = rule_elem re in
	  let asre = rule_elem asre in
	  bind re asre
      |	Ast.DisjRuleElem(res) ->
	  multibind (List.map rule_elem res) in
    rulefn all_functions k re

  (* not parameterisable, for now *)
  and forinfo fi =
    let k = function
	Ast.ForExp(e1,sem1) ->
	  let le1 = get_option expression e1 in
	  let lsem1 = string_mcode sem1 in
	  bind le1 lsem1
      | Ast.ForDecl decl -> annotated_decl decl in
    k fi

  (* not parameterisable, for now *)
  and pragmainfo pi =
    let k pi =
      match Ast.unwrap pi with
	Ast.PragmaString(s) -> string_mcode s
      | Ast.PragmaDots (dots) -> string_mcode dots in
    k pi

  (* not parameterizable for now... *)
  and define_parameters p =
    let k p =
      match Ast.unwrap p with
	Ast.NoParams -> option_default
      | Ast.DParams(lp,params,rp) ->
	  let llp = string_mcode lp in
	  let lparams = define_param_dots params in
	  let lrp = string_mcode rp in
	  multibind [llp; lparams; lrp] in
    k p

  and define_param p =
    let k p =
      match Ast.unwrap p with
	Ast.DParam(id) -> ident id
      | Ast.MetaDParamList(name,_,_,_,_) -> meta_mcode name
      | Ast.DPComma(comma) -> string_mcode comma
      | Ast.DPdots(d) -> string_mcode d
      | Ast.OptDParam(dp) -> define_param dp in
    define_paramfn all_functions k p

  (* discard the result, because the statement is assumed to be already
     represented elsewhere in the code *)
  and process_bef_aft s =
    match Ast.get_dots_bef_aft s with
      Ast.NoDots -> ()
    | Ast.DroppingBetweenDots(stm,ind) -> let _ = statement stm in ()
    | Ast.AddingBetweenDots(stm,ind) -> let _ = statement stm in ()

  and statement s =
    process_bef_aft s;
    let k s =
      match Ast.unwrap s with
	Ast.Seq(lbrace,body,rbrace) ->
	  let llbrace = rule_elem lbrace in
	  let lbody = statement_dots body in
	  let lrbrace = rule_elem rbrace in
	  multibind [llbrace; lbody; lrbrace]
      | Ast.IfThen(header,branch,_) ->
	  let lheader = rule_elem header in
	  let lbranch = statement branch in
	  bind lheader lbranch
      | Ast.IfThenElse(header,branch1,els,branch2,_) ->
	  let lheader = rule_elem header in
	  let lbranch1 = statement branch1 in
	  let lels = rule_elem els in
	  let lbranch2 = statement branch2 in
	  multibind [lheader; lbranch1; lels; lbranch2]
      | Ast.While(header,body,_) ->
	  let lheader = rule_elem header in
	  let lbody = statement body in
	  bind lheader lbody
      | Ast.Do(header,body,tail) ->
	  let lheader = rule_elem header in
	  let lbody = statement body in
	  let ltail = rule_elem tail in
	  multibind [lheader; lbody; ltail]
      | Ast.For(header,body,_) ->
	  let lheader = rule_elem header in
	  let lbody = statement body in
	  bind lheader lbody
      | Ast.Iterator(header,body,_) ->
	  let lheader = rule_elem header in
	  let lbody = statement body in
	  bind lheader lbody
      |	Ast.Switch(header,lb,decls,cases,rb) ->
	  let lheader = rule_elem header in
	  let llb = rule_elem lb in
	  let ldecls = statement_dots decls in
	  let lcases = multibind (List.map case_line cases) in
	  let lrb = rule_elem rb in
	  multibind [lheader; llb; ldecls; lcases; lrb]
      | Ast.Atomic(re) ->rule_elem re
      | Ast.Disj(stmt_dots_list) | Ast.Conj(stmt_dots_list) ->
	  multibind (List.map statement_dots stmt_dots_list)
      | Ast.Nest(starter,stmt_dots,ender,whn,_,_,_) ->
	  let lstarter = string_mcode starter in
	  let lstmt_dots = statement_dots stmt_dots in
	  let lender = string_mcode ender in
	  let lwhn = multibind
	    (List.map (whencode statement_dots statement) whn) in
	  multibind [lstarter; lstmt_dots; lender; lwhn]
      | Ast.FunDecl(header,lbrace,body,rbrace,_) ->
	  let lheader = rule_elem header in
	  let lbraces = rule_elem lbrace in
	  let lbody = statement_dots body in
	  let lrbrace = rule_elem rbrace in
	  multibind [lheader; lbraces; lbody; lrbrace]
      | Ast.Define(header,body) ->
	  let lheader = rule_elem header in
	  let lbody = statement_dots body in
	  bind lheader lbody
      |	Ast.AsStmt(stm,asstm) ->
	  let lstm = statement stm in
	  let lasstm = statement asstm in
	  bind lstm lasstm
      | Ast.Dots(d,whn,_,_) ->
	  let ld = string_mcode d in
	  let lwhn = multibind
	    (List.map (whencode statement_dots statement) whn) in
	  bind ld lwhn
      | Ast.OptStm(stmt) -> statement stmt in
    stmtfn all_functions k s

  and fninfo = function
      Ast.FStorage(stg) -> storage_mcode stg
    | Ast.FType(ty) -> fullType ty
    | Ast.FInline(inline) -> string_mcode inline
    | Ast.FAttr(attr) -> attribute attr

  and attribute a =
    let k a =
      match Ast.unwrap a with
        Ast.Attribute(arg) -> attr_arg arg in
          attributefn all_functions k a

  and attr_arg a =
    let k a =
      match Ast.unwrap a with
        Ast.AttrName(arg) -> string_mcode arg
      | Ast.MetaAttr(name,_,_,_) -> meta_mcode name in
    attr_argfn all_functions k a

  and whencode notfn alwaysfn = function
      Ast.WhenNot a -> notfn a
    | Ast.WhenAlways a -> alwaysfn a
    | Ast.WhenModifier(_) -> option_default
    | Ast.WhenNotTrue(e) -> rule_elem e
    | Ast.WhenNotFalse(e) -> rule_elem e

  and case_line c =
    let k c =
      match Ast.unwrap c with
	Ast.CaseLine(header,code) ->
	  let lheader = rule_elem header in
	  let lcode = statement_dots code in
	  bind lheader lcode
      |	Ast.OptCase(case) -> case_line case in
    casefn all_functions k c

  and exec_code e =
    (* not configurable *)
    match Ast.unwrap e with
      Ast.ExecEval(colon,id) ->
	let lcolon = string_mcode colon in
	let lid = expression id in
	bind lcolon lid
    | Ast.ExecToken(tok) -> string_mcode tok
    | Ast.ExecDots(dots) -> string_mcode dots

  and top_level t =
    let k t =
      match Ast.unwrap t with
	Ast.FILEINFO(old_file,new_file) ->
	  bind (string_mcode old_file) (string_mcode new_file)
      | Ast.NONDECL(stmt) -> statement stmt
      | Ast.CODE(stmt_dots) -> statement_dots stmt_dots
      | Ast.ERRORWORDS(exps) -> multibind (List.map expression exps) in
    topfn all_functions k t

  and anything a =
    let k = function
	(*in many cases below, the thing is not even mcode, so we do nothing*)
	Ast.FullTypeTag(ft) -> fullType ft
      | Ast.BaseTypeTag(bt) -> option_default
      | Ast.StructUnionTag(su) -> option_default
      | Ast.SignTag(sgn) -> option_default
      | Ast.IdentTag(id) -> ident id
      | Ast.ExpressionTag(exp) -> expression exp
      | Ast.ConstantTag(cst) -> option_default
      | Ast.UnaryOpTag(unop) -> option_default
      | Ast.AssignOpTag(asgnop) -> assignOp asgnop
      | Ast.SimpleAssignOpTag _ -> option_default
      | Ast.OpAssignOpTag _ -> option_default
      | Ast.FixOpTag(fixop) -> option_default
      | Ast.BinaryOpTag(binop) -> binaryOp binop
      | Ast.ArithOpTag(arithop) -> option_default
      | Ast.LogicalOpTag(logop) -> option_default
      | Ast.DeclarationTag(decl) -> declaration decl
      | Ast.FieldTag(decl) -> field decl
      | Ast.EnumDeclTag(decl) -> enum_decl decl
      | Ast.InitTag(ini) -> initialiser ini
      | Ast.StorageTag(stg) -> option_default
      | Ast.IncFileTag(stg) -> option_default
      | Ast.Rule_elemTag(rule) -> rule_elem rule
      | Ast.StatementTag(rule) -> statement rule
      | Ast.ForInfoTag(rule) -> forinfo rule
      | Ast.CaseLineTag(case) -> case_line case
      | Ast.StringFragmentTag(frag) -> string_fragment frag
      | Ast.AttributeTag(attr) -> attribute attr
      | Ast.AttrArgTag(arg) -> attr_arg arg
      | Ast.ConstVolTag(cv) -> option_default
      | Ast.Token(tok,info) -> option_default
      | Ast.Directive(str) -> option_default
      | Ast.Code(cd) -> top_level cd
      | Ast.ExprDotsTag(ed) -> expression_dots ed
      | Ast.ParamDotsTag(pd) -> parameter_dots pd
      | Ast.StmtDotsTag(sd) -> statement_dots sd
      | Ast.AnnDeclDotsTag(sd) -> annotated_decl_dots sd
      | Ast.AnnFieldDotsTag(sd) -> annotated_field_dots sd
      | Ast.EnumDeclDotsTag(sd) -> enum_decl_dots sd
      | Ast.DefParDotsTag(sd) -> define_param_dots sd
      | Ast.TypeCTag(ty) -> typeC ty
      | Ast.ParamTag(param) -> parameterTypeDef param
      | Ast.SgrepStartTag(tok) -> option_default
      | Ast.SgrepEndTag(tok) -> option_default in
    anyfn all_functions k a

  and all_functions =
    {combiner_ident = ident;
      combiner_expression = expression;
      combiner_fragment = string_fragment;
      combiner_format = string_format;
      combiner_assignOp = assignOp;
      combiner_binaryOp = binaryOp;
      combiner_fullType = fullType;
      combiner_typeC = typeC;
      combiner_declaration = declaration;
      combiner_field = field;
      combiner_ann_field = annotated_field;
      combiner_enumdecl = enum_decl;
      combiner_initialiser = initialiser;
      combiner_parameter = parameterTypeDef;
      combiner_parameter_list = parameter_dots;
      combiner_rule_elem = rule_elem;
      combiner_statement = statement;
      combiner_case_line = case_line;
      combiner_attribute = attribute;
      combiner_attr_arg = attr_arg;
      combiner_top_level = top_level;
      combiner_anything = anything;
      combiner_expression_dots = expression_dots;
      combiner_statement_dots = statement_dots;
      combiner_anndecl_dots = annotated_decl_dots;
      combiner_annfield_dots = annotated_field_dots;
      combiner_enumdecl_dots = enum_decl_dots;
      combiner_initialiser_dots = initialiser_dots} in
  all_functions

(* ---------------------------------------------------------------------- *)

type 'a inout = 'a -> 'a (* for specifying the type of rebuilder *)

type rebuilder =
    {rebuilder_ident : Ast.ident inout;
      rebuilder_expression : Ast.expression inout;
      rebuilder_fragment : Ast.string_fragment inout;
      rebuilder_format : Ast.string_format inout;
      rebuilder_assignOp : Ast_cocci.assignOp inout;
      rebuilder_binaryOp : Ast_cocci.binaryOp inout;
      rebuilder_fullType : Ast.fullType inout;
      rebuilder_typeC : Ast.typeC inout;
      rebuilder_declaration : Ast.declaration inout;
      rebuilder_field : Ast.field inout;
      rebuilder_ann_field : Ast.annotated_field inout;
      rebuilder_enumdecl : Ast_cocci.enum_decl inout;
      rebuilder_initialiser : Ast.initialiser inout;
      rebuilder_parameter : Ast.parameterTypeDef inout;
      rebuilder_parameter_list : Ast.parameter_list inout;
      rebuilder_statement : Ast.statement inout;
      rebuilder_case_line : Ast.case_line inout;
      rebuilder_attribute : Ast.attr inout;
      rebuilder_attr_arg : Ast.attr_arg inout;
      rebuilder_rule_elem : Ast.rule_elem inout;
      rebuilder_top_level : Ast.top_level inout;
      rebuilder_expression_dots : Ast.expression Ast.dots inout;
      rebuilder_statement_dots : Ast.statement Ast.dots inout;
      rebuilder_anndecl_dots : Ast.annotated_decl Ast.dots inout;
      rebuilder_annfield_dots : Ast.annotated_field Ast.dots inout;
      rebuilder_enumdecl_dots : Ast.enum_decl Ast.dots inout;
      rebuilder_initialiser_dots : Ast.initialiser Ast.dots inout;
      rebuilder_define_param_dots : Ast.define_param Ast.dots inout;
      rebuilder_define_param : Ast.define_param inout;
      rebuilder_define_parameters : Ast.define_parameters inout;
      rebuilder_anything : Ast.anything inout}

type 'mc rmcode = 'mc Ast.mcode inout
type 'cd rcode = rebuilder -> ('cd inout) -> 'cd inout


let rebuilder
    meta_mcode string_mcode const_mcode simpleassign_mcode opassign_mcode
    fix_mcode unary_mcode
    arithop_mcode logicalop_mcode cv_mcode sign_mcode struct_mcode
    storage_mcode inc_file_mcode
    expdotsfn paramdotsfn stmtdotsfn anndecldotsfn annfielddotsfn
    enumdecldotsfn initdotsfn
    identfn exprfn fragfn fmtfn assignOpfn binaryOpfn ftfn tyfn initfn
    paramfn define_paramfn declfn annotated_declfn fieldfn annotated_fieldfn
    enum_declfn rulefn stmtfn casefn attributefn attr_argfn topfn anyfn =
  let get_option f = function
      Some x -> Some (f x)
    | None -> None in

  let dotsfn param default all_functions arg =
    let k d = Ast.rewrap d (List.map default (Ast.unwrap d)) in
    param all_functions k arg in

  let strdotsfn all_functions k arg = k arg in
  let ecdotsfn all_functions k arg = k arg in

  let rec expression_dots d = dotsfn expdotsfn expression all_functions d
  and parameter_dots d = dotsfn paramdotsfn parameterTypeDef all_functions d
  and statement_dots d = dotsfn stmtdotsfn statement all_functions d
  and annotated_decl_dots d =
    dotsfn anndecldotsfn annotated_decl all_functions d
  and annotated_field_dots d =
    dotsfn annfielddotsfn annotated_field all_functions d
  and enum_decl_dots d =
    dotsfn enumdecldotsfn enum_decl all_functions d
  and initialiser_dots d = dotsfn initdotsfn initialiser all_functions d
  and string_fragment_dots d = dotsfn strdotsfn string_fragment all_functions d
  and exec_code_dots d = dotsfn ecdotsfn exec_code all_functions d

  and ident i =
    let k i =
      Ast.rewrap i
	(match Ast.unwrap i with
	  Ast.Id(name) -> Ast.Id(string_mcode name)
	| Ast.MetaId(name,constraints,keep,inherited) ->
	    Ast.MetaId(meta_mcode name,constraints,keep,inherited)
	| Ast.MetaFunc(name,constraints,keep,inherited) ->
	    Ast.MetaFunc(meta_mcode name,constraints,keep,inherited)
	| Ast.MetaLocalFunc(name,constraints,keep,inherited) ->
	    Ast.MetaLocalFunc(meta_mcode name,constraints,keep,inherited)
	| Ast.AsIdent(id,asid) -> Ast.AsIdent(ident id,ident asid)
	| Ast.DisjId(id_list) -> Ast.DisjId(List.map ident id_list)
	| Ast.ConjId(id_list) -> Ast.ConjId(List.map ident id_list)
	| Ast.OptIdent(id) -> Ast.OptIdent(ident id)) in
    identfn all_functions k i

  and expression e =
    let k e =
      Ast.rewrap e
	(match Ast.unwrap e with
	  Ast.Ident(id) -> Ast.Ident(ident id)
	| Ast.Constant(const) -> Ast.Constant(const_mcode const)
	| Ast.StringConstant(lq,str,rq,sz) ->
	    let llq = string_mcode lq in
	    let lstr = string_fragment_dots str in
	    let lrq = string_mcode rq in
	    Ast.StringConstant(llq, lstr, lrq, sz)
	| Ast.FunCall(fn,lp,args,rp) ->
	    let lfn = expression fn in
	    let llp = string_mcode lp in
	    let largs = expression_dots args in
	    let lrp = string_mcode rp in
	    Ast.FunCall(lfn, llp, largs, lrp)
	| Ast.Assignment(left,op,right,simple) ->
	    let lleft = expression left in
	    let lop = assignOp op in
	    let lright = expression right in
	    Ast.Assignment(lleft, lop, lright, simple)
	| Ast.Sequence(left,op,right) ->
	    let lleft = expression left in
	    let lop = string_mcode op in
	    let lright = expression right in
	    Ast.Sequence(lleft, lop, lright)
	| Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
	    let lexp1 = expression exp1 in
	    let lwhy = string_mcode why in
	    let lexp2 = get_option expression exp2 in
	    let lcolon = string_mcode colon in
	    let lexp3 = expression exp3 in
	    Ast.CondExpr(lexp1, lwhy, lexp2, lcolon, lexp3)
	| Ast.Postfix(exp,op) ->
	    let lexp = expression exp in
	    let lop = fix_mcode op in
	    Ast.Postfix(lexp, lop)
	| Ast.Infix(exp,op) ->
	    let lexp = expression exp in
	    let lop = fix_mcode op in
	    Ast.Infix(lexp, lop)
	| Ast.Unary(exp,op) ->
	    let lexp = expression exp in
	    let lop = unary_mcode op in
	    Ast.Unary(lexp, lop)
	| Ast.Binary(left,op,right) ->
	    let lleft = expression left in
	    let lop = binaryOp op in
	    let lright = expression right in
	    Ast.Binary(lleft, lop, lright)
	| Ast.Nested(left,op,right) ->
	    let lleft = expression left in
	    let lop = binaryOp op in
	    let lright = expression right in
	    Ast.Nested(lleft, lop, lright)
	| Ast.Paren(lp,exp,rp) ->
	    let llp = string_mcode lp in
	    let lexp = expression exp in
	    let lrp = string_mcode rp in
	    Ast.Paren(llp, lexp, lrp)
	| Ast.ArrayAccess(exp1,lb,exp2,rb) ->
	    let lexp1 = expression exp1 in
	    let llb = string_mcode lb in
	    let lexp2 = expression exp2 in
	    let lrb = string_mcode rb in
	    Ast.ArrayAccess(lexp1, llb, lexp2, lrb)
	| Ast.RecordAccess(exp,pt,field) ->
	    let lexp = expression exp in
	    let lpt = string_mcode pt in
	    let lfield = ident field in
	    Ast.RecordAccess(lexp, lpt, lfield)
	| Ast.RecordPtAccess(exp,ar,field) ->
	    let lexp = expression exp in
	    let lar = string_mcode ar in
	    let lfield = ident field in
	    Ast.RecordPtAccess(lexp, lar, lfield)
	| Ast.Cast(lp,ty,attr,rp,exp) ->
	    let llp = string_mcode lp in
	    let lty = fullType ty in
	    let lattr = List.map attribute attr in
	    let lrp = string_mcode rp in
	    let lexp = expression exp in
	    Ast.Cast(llp, lty, lattr, lrp, lexp)
	| Ast.SizeOfExpr(szf,exp) ->
	    let lszf = string_mcode szf in
	    let lexp = expression exp in
	    Ast.SizeOfExpr(lszf, lexp)
	| Ast.SizeOfType(szf,lp,ty,rp) ->
	    let lszf = string_mcode szf in
	    let llp = string_mcode lp in
	    let lty = fullType ty in
	    let lrp = string_mcode rp in
	    Ast.SizeOfType(lszf, llp, lty, lrp)
	| Ast.TypeExp(ty) -> Ast.TypeExp(fullType ty)
	| Ast.Constructor(lp,ty,rp,init) ->
	    let llp = string_mcode lp in
	    let lty = fullType ty in
	    let lrp = string_mcode rp in
	    let linit = initialiser init in
	    Ast.Constructor(llp, lty, lrp, linit)
	| Ast.MetaErr(name,constraints,keep,inherited) ->
	    Ast.MetaErr(meta_mcode name,constraints,keep,inherited)
	| Ast.MetaExpr(name,constraints,keep,ty,form,inherited,bitfield) ->
	    Ast.MetaExpr
	      (meta_mcode name,constraints,keep,ty,form,inherited,bitfield)
	| Ast.MetaExprList(name,lenname_inh,constraints,keep,inherited) ->
	    Ast.MetaExprList
	      (meta_mcode name,lenname_inh,constraints,keep,inherited)
	| Ast.AsExpr(exp,asexp) ->
	    let lexp = expression exp in
	    let lasexp = expression asexp in
	    Ast.AsExpr(lexp, lasexp)
	| Ast.AsSExpr(exp,asstm) -> 
	    let lexp = expression exp in
	    let lasstm = rule_elem asstm in
	    Ast.AsSExpr(lexp, lasstm)
	| Ast.EComma(cm) -> Ast.EComma(string_mcode cm)
	| Ast.DisjExpr(exp_list) -> Ast.DisjExpr(List.map expression exp_list)
	| Ast.ConjExpr(exp_list) -> Ast.ConjExpr(List.map expression exp_list)
	| Ast.NestExpr(starter,expr_dots,ender,whncode,multi) ->
	    let lstarter = string_mcode starter in
	    let lexpr_dots = expression_dots expr_dots in
	    let lender = string_mcode ender in
	    let lwhncode = get_option expression whncode in
	    Ast.NestExpr(lstarter, lexpr_dots, lender, lwhncode, multi)
	| Ast.Edots(dots,whncode) ->
	    let ldots = string_mcode dots in
	    let lwhncode = get_option expression whncode in
	    Ast.Edots(ldots, lwhncode)
	| Ast.OptExp(exp) -> Ast.OptExp(expression exp)) in
    exprfn all_functions k e

  and string_fragment e =
    let k e =
      Ast.rewrap e
	(match Ast.unwrap e with
	  Ast.ConstantFragment(str) -> Ast.ConstantFragment(string_mcode str)
	| Ast.FormatFragment(pct,fmt) ->
	    let lpct = string_mcode pct in
	    let lfmt = string_format fmt in
	    Ast.FormatFragment(lpct, lfmt)
	| Ast.Strdots dots -> Ast.Strdots (string_mcode dots)
	| Ast.MetaFormatList(pct,name,lenname,constraints,keep,inherited) ->
	    let lpct = string_mcode pct in
	    let lname = meta_mcode name in
	    Ast.MetaFormatList
	      (lpct, lname, lenname, constraints, keep, inherited)) in
    fragfn all_functions k e

  and string_format e =
    let k e =
      Ast.rewrap e
	(match Ast.unwrap e with
	  Ast.ConstantFormat(str) -> Ast.ConstantFormat(string_mcode str)
	| Ast.MetaFormat(name,constraints,keep,inherited) ->
	    Ast.MetaFormat(meta_mcode name,constraints,keep,inherited)) in
    fmtfn all_functions k e

  and assignOp op =
    let k op =
      Ast.rewrap op
        (match Ast.unwrap op with
          Ast.SimpleAssign o -> Ast.SimpleAssign (simpleassign_mcode o)
        | Ast.OpAssign o -> Ast.OpAssign (opassign_mcode o)
        | Ast.MetaAssign (mv,x,y,z) -> Ast.MetaAssign ((meta_mcode mv),x,y,z)
        ) in
    assignOpfn all_functions k op

  and binaryOp op =
    let k op =
      Ast.rewrap op
        (match Ast.unwrap op with
          Ast.Arith o -> Ast.Arith (arithop_mcode o)
        | Ast.Logical o -> Ast.Logical (logicalop_mcode o)
        | Ast.MetaBinary (mv,x,y,z) -> Ast.MetaBinary ((meta_mcode mv),x,y,z)
        ) in
    binaryOpfn all_functions k op

  and fullType ft =
    let k ft =
      Ast.rewrap ft
	(match Ast.unwrap ft with
	  Ast.Type(allminus,cv,ty) ->
	    let lcv = get_option cv_mcode cv in
	    let lty = typeC ty in
	    Ast.Type (allminus, lcv, lty)
	| Ast.AsType(ty,asty) ->
	    let lty = fullType ty in
	    let lasty = fullType asty in
	    Ast.AsType(lty, lasty)
	| Ast.DisjType(types) -> Ast.DisjType(List.map fullType types)
	| Ast.ConjType(types) -> Ast.ConjType(List.map fullType types)
	| Ast.OptType(ty) -> Ast.OptType(fullType ty)) in
    ftfn all_functions k ft

  and typeC ty =
    let k ty =
      Ast.rewrap ty
	(match Ast.unwrap ty with
	  Ast.BaseType(ty,strings) ->
	    Ast.BaseType (ty, List.map string_mcode strings)
	| Ast.SignedT(sgn,ty) ->
	    let lsgn = sign_mcode sgn in
	    let lty = get_option typeC ty in
	    Ast.SignedT(lsgn, lty)
	| Ast.Pointer(ty,star) ->
	    let lty = fullType ty in
	    let lstar = string_mcode star in
	    Ast.Pointer (lty, lstar)
        | Ast.ParenType(lp,ty,rp) ->
            let llp = string_mcode lp in
            let lty = fullType ty in
            let lrp = string_mcode rp in
            Ast.ParenType(llp,lty,lrp)
        | Ast.FunctionType(ty,lp,params,rp) ->
            let lty = fullType ty in
            let llp = string_mcode lp in
            let lparams = parameter_dots params in
            let lrp = string_mcode rp in
            Ast.FunctionType(lty,llp,lparams,lrp)
	| Ast.Array(ty,lb,size,rb) ->
	    let lty = fullType ty in
	    let llb = string_mcode lb in
	    let lsize = get_option expression size in
	    let lrb = string_mcode rb in
	    Ast.Array(lty, llb, lsize, lrb)
      | Ast.Decimal(dec,lp,length,comma,precision_opt,rp) ->
	    let ldec = string_mcode dec in
	    let llp = string_mcode lp in
	    let llength = expression length in
	    let lcomma = get_option string_mcode comma in
	    let lprecision_opt = get_option expression precision_opt in
	    let lrp = string_mcode rp in
	  Ast.Decimal(ldec, llp, llength, lcomma, lprecision_opt, lrp)
	| Ast.EnumName(kind,name) ->
	    let lkind = string_mcode kind in
	    let lname = get_option ident name in
	    Ast.EnumName(lkind, lname)
	| Ast.EnumDef(ty,lb,ids,rb) ->
	    let lty = fullType ty in
	    let llb = string_mcode lb in
	    let lids = enum_decl_dots ids in
	    let lrb = string_mcode rb in
	    Ast.EnumDef (lty, llb, lids, lrb)
	| Ast.StructUnionName(kind,name) ->
	    let lkind = struct_mcode kind in
	    let lname = get_option ident name in
	    Ast.StructUnionName (lkind, lname)
	| Ast.StructUnionDef(ty,lb,decls,rb) ->
	    let lty = fullType ty in
	    let llb = string_mcode lb in
	    let ldecls = annotated_field_dots decls in
	    let lrb = string_mcode rb in
	    Ast.StructUnionDef (lty, llb, ldecls, lrb)
	| Ast.TypeOfExpr(tf,lp,e,rp) ->
	    let ltf = string_mcode tf in
	    let llp = string_mcode lp in
	    let le = expression e in
	    let lrp = string_mcode rp in
	    Ast.TypeOfExpr(ltf, llp, le, lrp)
	| Ast.TypeOfType(tf,lp,ty,rp) ->
	    let ltf = string_mcode tf in
	    let llp = string_mcode lp in
	    let lty = fullType ty in
	    let lrp = string_mcode rp in
	    Ast.TypeOfType(ltf, llp, lty, lrp)
	| Ast.TypeName(name) -> Ast.TypeName(string_mcode name)
	| Ast.AutoType(auto) -> Ast.AutoType(string_mcode auto)
	| Ast.MetaType(name,cstr,keep,inherited) ->
	    Ast.MetaType(meta_mcode name,cstr,keep,inherited)) in
    tyfn all_functions k ty

  and declaration d =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.MetaDecl(name,constraints,keep,inherited) ->
	    Ast.MetaDecl(meta_mcode name,constraints,keep,inherited)
	| Ast.AsDecl(decl,asdecl) ->
	    let ldecl = declaration decl in
	    let lasdecl = declaration asdecl in
	    Ast.AsDecl(ldecl, lasdecl)
	| Ast.Init(stg,ty,id,attr,eq,ini,sem) ->
	    let lstg = get_option storage_mcode stg in
	    let lty = fullType ty in
	    let lid = ident id in
	    let lattr = List.map attribute attr in
	    let leq = string_mcode eq in
	    let lini = initialiser ini in
	    let lsem = string_mcode sem in
	    Ast.Init(lstg, lty, lid, lattr, leq, lini, lsem)
	| Ast.UnInit(stg,ty,id,attr,sem) ->
	    let lstg = get_option storage_mcode stg in
	    let lty = fullType ty in
	    let lid = ident id in
	    let lattr = List.map attribute attr in
	    let lsem = string_mcode sem in
	    Ast.UnInit(lstg, lty, lid, lattr, lsem)
	| Ast.FunProto(fi,name,lp,params,va,rp,sem) ->
	    let lfi = List.map fninfo fi in
	    let lname = ident name in
	    let llp = string_mcode lp in
	    let lparams = parameter_dots params in
            let lva = match va with
              | None -> None
              | Some (comma,ellipsis) ->
		  Some (string_mcode comma,string_mcode ellipsis) in
	    let lrp = string_mcode rp in
	    let lsem = string_mcode sem in
	    Ast.FunProto(lfi,lname,llp,lparams,lva,lrp,lsem)
	| Ast.MacroDecl(stg,name,lp,args,rp,attr,sem) ->
	    let lstg = get_option storage_mcode stg in
	    let lname = ident name in
	    let llp = string_mcode lp in
	    let largs = expression_dots args in
	    let lrp = string_mcode rp in
	    let lattr = List.map attribute attr in
	    let lsem = string_mcode sem in
	    Ast.MacroDecl(lstg, lname, llp, largs, lrp, lattr, lsem)
	| Ast.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) ->
	    let lstg = get_option storage_mcode stg in
	    let lname = ident name in
	    let llp = string_mcode lp in
	    let largs = expression_dots args in
	    let lrp = string_mcode rp in
	    let leq = string_mcode eq in
	    let lini = initialiser ini in
	    let lsem = string_mcode sem in
	    Ast.MacroDeclInit(lstg, lname, llp, largs, lrp, leq, lini, lsem)
	| Ast.TyDecl(ty,attr,sem) ->
	    let lty = fullType ty in
	    let lattr = List.map attribute attr in
	    let lsem = string_mcode sem in
	    Ast.TyDecl(lty, lattr, lsem)
	| Ast.Typedef(stg,ty,id,sem) ->
	    let lstg = string_mcode stg in
	    let lty = fullType ty in
	    let lid = typeC id in
	    let lsem = string_mcode sem in
	    Ast.Typedef(lstg, lty, lid, lsem)
	| Ast.DisjDecl(decls) -> Ast.DisjDecl(List.map declaration decls)
	| Ast.ConjDecl(decls) -> Ast.ConjDecl(List.map declaration decls)
	| Ast.OptDecl(decl) -> Ast.OptDecl(declaration decl)) in
    declfn all_functions k d

  and annotated_decl d =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.DElem(bef,allminus,decl) ->
	    Ast.DElem(bef,allminus,declaration decl)) in
    annotated_declfn all_functions k d

  and field d =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.MetaField(name,constraints,keep,inherited) ->
	    Ast.MetaField(meta_mcode name,constraints,keep,inherited)
	| Ast.MetaFieldList(name,lenname_inh,constraints,keep,inherited) ->
	    Ast.MetaFieldList
	      (meta_mcode name,lenname_inh,constraints,keep,inherited)
	| Ast.Field(ty,id,bf,sem) ->
	    let lty = fullType ty in
	    let lid = Common.map_option ident id in
	    let bitfield (c, e) =
	      let lc = string_mcode c in
	      let le = expression e in
	      (lc, le) in
	    let lbf = Common.map_option bitfield bf in
	    let lsem = string_mcode sem in
	    Ast.Field(lty, lid, lbf, lsem)) in
    fieldfn all_functions k d

  and annotated_field d =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.FElem(bef, allminus, decl) ->
	    Ast.FElem(bef, allminus, field decl)
	| Ast.Fdots(dots,whncode) ->
	    let ldots = string_mcode dots in
	    let lwhncode = get_option field whncode in
	    Ast.Fdots(ldots, lwhncode)
	| Ast.DisjField(decls) ->
	    Ast.DisjField(List.map annotated_field decls)
	| Ast.ConjField(decls) ->
	    Ast.ConjField(List.map annotated_field decls)
	| Ast.OptField(decl) -> Ast.OptField(annotated_field decl)) in
    annotated_fieldfn all_functions k d

  and enum_decl d =
    let k d =
      Ast.rewrap d
        (match Ast.unwrap d with
         Ast.Enum(name,enum_val) ->
            let lname = ident name in
            (match enum_val with
              None -> Ast.Enum(lname,None)
            | Some(eq,eval) ->
                let leq = string_mcode eq in
                let leval = expression eval in
                Ast.Enum(lname,Some(leq,leval)))
        | Ast.EnumComma(cm) ->
          let lcm = string_mcode cm in
          Ast.EnumComma(lcm)
	| Ast.EnumDots(dots,whncode) ->
	  let ldots = string_mcode dots in
	  let lwhncode = get_option enum_decl whncode in
	  Ast.EnumDots(ldots, lwhncode)) in
    enum_declfn all_functions k d

  and initialiser i =
    let k i =
      Ast.rewrap i
	(match Ast.unwrap i with
	  Ast.MetaInit(name,constraints,keep,inherited) ->
	    Ast.MetaInit(meta_mcode name,constraints,keep,inherited)
	| Ast.MetaInitList(name,lenname_inh,constraints,keep,inherited) ->
	    Ast.MetaInitList
	      (meta_mcode name,lenname_inh,constraints,keep,inherited)
	| Ast.AsInit(ini,asini) ->
	    let lini = initialiser ini in
	    let lasinit = initialiser asini in
	    Ast.AsInit(lini, lasinit)
	| Ast.InitExpr(exp) -> Ast.InitExpr(expression exp)
	| Ast.ArInitList(lb,initlist,rb) ->
	    let llb = string_mcode lb in
	    let linitlist = initialiser_dots initlist in
	    let lrb = string_mcode rb in
	    Ast.ArInitList(llb, linitlist, lrb)
	| Ast.StrInitList(allminus,lb,initlist,rb,whncode) ->
	    let llb = string_mcode lb in
	    let linitlist = List.map initialiser initlist in
	    let lrb = string_mcode rb in
	    let lwhncode = List.map initialiser whncode in
	    Ast.StrInitList(allminus,llb, linitlist, lrb, lwhncode)
	| Ast.InitGccName(name,eq,ini) ->
	    let lname = ident name in
	    let leq = string_mcode eq in
	    let lini = initialiser ini in
	    Ast.InitGccName(lname, leq, lini)
	| Ast.InitGccExt(designators,eq,ini) ->
	    let ldesignators = List.map designator designators in
	    let leq = string_mcode eq in
	    let lini = initialiser ini in
	    Ast.InitGccExt(ldesignators, leq, lini)
	| Ast.IComma(cm) -> Ast.IComma(string_mcode cm)
	| Ast.Idots(dots,whncode) ->
	    let ldots = string_mcode dots in
	    let lwhncode = get_option initialiser whncode in
	    Ast.Idots(ldots, lwhncode)
	| Ast.OptIni(i) -> Ast.OptIni(initialiser i)) in
    initfn all_functions k i

  and designator = function
      Ast.DesignatorField(dot,id) ->
	let ldot = string_mcode dot in
	let lid = ident id in
	Ast.DesignatorField(ldot, lid)
    | Ast.DesignatorIndex(lb,exp,rb) ->
	let llb = string_mcode lb in
	let lexp = expression exp in
	let lrb = string_mcode rb in
	Ast.DesignatorIndex(llb, lexp, lrb)
    | Ast.DesignatorRange(lb,min,dots,max,rb) ->
	let llb = string_mcode lb in
	let lmin = expression min in
	let ldots = string_mcode dots in
	let lmax = expression max in
	let lrb = string_mcode rb in
	Ast.DesignatorRange(llb, lmin, ldots, lmax, lrb)

  and parameterTypeDef p =
    let k p =
      Ast.rewrap p
	(match Ast.unwrap p with
	  Ast.VoidParam(ty,attr) ->
            Ast.VoidParam(fullType ty,List.map attribute attr)
	| Ast.Param(ty,id,attr) ->
            Ast.Param
              (fullType ty, get_option ident id,List.map attribute attr)
	| Ast.MetaParam(name,constraints,keep,inherited) ->
	    Ast.MetaParam(meta_mcode name,constraints,keep,inherited)
	| Ast.MetaParamList(name,lenname_inh,constraints,keep,inherited) ->
	    Ast.MetaParamList
	      (meta_mcode name,lenname_inh,constraints,keep,inherited)
	| Ast.AsParam(p,asexp) ->
	    let lp = parameterTypeDef p in
	    let lasexp = expression asexp in
	    Ast.AsParam(lp, lasexp)
	| Ast.PComma(cm) -> Ast.PComma(string_mcode cm)
	| Ast.Pdots(dots) -> Ast.Pdots(string_mcode dots)
	| Ast.OptParam(param) -> Ast.OptParam(parameterTypeDef param)) in
    paramfn all_functions k p

  and rule_elem re =
    let k re =
      Ast.rewrap re
	(match Ast.unwrap re with
	  Ast.FunHeader(bef,allminus,fi,name,lp,params,va,rp) ->
	    let lfi = List.map fninfo fi in
	    let lname = ident name in
	    let llp = string_mcode lp in
	    let lparams = parameter_dots params in
            let lva = match va with
              | None -> None
              | Some (comma,ellipsis) -> Some(string_mcode comma,string_mcode ellipsis) in
	    let lrp = string_mcode rp in
	    Ast.FunHeader(bef,allminus, lfi, lname, llp, lparams, lva, lrp)
	| Ast.Decl decl -> Ast.Decl (annotated_decl decl)
	| Ast.SeqStart(brace) -> Ast.SeqStart(string_mcode brace)
	| Ast.SeqEnd(brace) -> Ast.SeqEnd(string_mcode brace)
	| Ast.ExprStatement(exp,sem) ->
	    let lexp = get_option expression exp in
	    let lsem = string_mcode sem in
	    Ast.ExprStatement (lexp, lsem)
	| Ast.IfHeader(iff,lp,exp,rp) ->
	    let liff = string_mcode iff in
	    let llp = string_mcode lp in
	    let lexp = expression exp in
	    let lrp = string_mcode rp in
	    Ast.IfHeader(liff, llp, lexp, lrp)
	| Ast.Else(els) -> Ast.Else(string_mcode els)
	| Ast.WhileHeader(whl,lp,exp,rp) ->
	    let lwhl = string_mcode whl in
	    let llp = string_mcode lp in
	    let lexp = expression exp in
	    let lrp = string_mcode rp in
	    Ast.WhileHeader(lwhl, llp, lexp, lrp)
	| Ast.DoHeader(d) -> Ast.DoHeader(string_mcode d)
	| Ast.WhileTail(whl,lp,exp,rp,sem) ->
	    let lwhl = string_mcode whl in
	    let llp = string_mcode lp in
	    let lexp = expression exp in
	    let lrp = string_mcode rp in
	    let lsem = string_mcode sem in
	    Ast.WhileTail(lwhl, llp, lexp, lrp, lsem)
	| Ast.ForHeader(fr,lp,first,e2,sem2,e3,rp) ->
	    let lfr = string_mcode fr in
	    let llp = string_mcode lp in
	    let lfirst = forinfo first in
	    let le2 = get_option expression e2 in
	    let lsem2 = string_mcode sem2 in
	    let le3 = get_option expression e3 in
	    let lrp = string_mcode rp in
	    Ast.ForHeader(lfr, llp, lfirst, le2, lsem2, le3, lrp)
	| Ast.IteratorHeader(whl,lp,args,rp) ->
	    let lnm = ident whl in
	    let llp = string_mcode lp in
	    let largs = expression_dots args in
	    let lrp = string_mcode rp in
	    Ast.IteratorHeader(lnm, llp, largs, lrp)
	| Ast.SwitchHeader(switch,lp,exp,rp) ->
	    let lswitch = string_mcode switch in
	    let llp = string_mcode lp in
	    let lexp = expression exp in
	    let lrp = string_mcode rp in
	    Ast.SwitchHeader(lswitch, llp, lexp, lrp)
	| Ast.Break(br,sem) ->
	    let lbr = string_mcode br in
	    let lsem = string_mcode sem in
	    Ast.Break(lbr, lsem)
	| Ast.Continue(cont,sem) ->
	    let lcont = string_mcode cont in
	    let lsem = string_mcode sem in
	    Ast.Continue(lcont, lsem)
	| Ast.Label(l,dd) ->
	    let ll = ident l in
	    let ldd = string_mcode dd in
	    Ast.Label(ll, ldd)
	| Ast.Goto(goto,l,sem) ->
	    let lgoto = string_mcode goto in
	    let ll = ident l in
	    let lsem = string_mcode sem in
	    Ast.Goto(lgoto, ll, lsem)
	| Ast.Return(ret,sem) ->
	    let lret = string_mcode ret in
	    let lsem = string_mcode sem in
	    Ast.Return(lret, lsem)
	| Ast.ReturnExpr(ret,exp,sem) ->
	    let lret = string_mcode ret in
	    let lexp =  expression exp in
	    let lsem = string_mcode sem in
	    Ast.ReturnExpr(lret, lexp, lsem)
	| Ast.Exec(exec,lang,code,sem) ->
	    let lexec = string_mcode exec in
	    let lland = string_mcode lang in
	    let lcode = exec_code_dots code in
	    let lsem = string_mcode sem in
	    Ast.Exec(lexec, lland, lcode, lsem)
	| Ast.MetaStmt(name,constraints,keep,seqible,inherited) ->
	    Ast.MetaStmt(meta_mcode name,constraints,keep,seqible,inherited)
	| Ast.MetaStmtList(name,lenname_inh,constraints,keep,inherited) ->
	    Ast.MetaStmtList
	      (meta_mcode name,lenname_inh,constraints,keep,inherited)
	| Ast.MetaRuleElem(name,constraints,keep,inherited) ->
	    Ast.MetaRuleElem(meta_mcode name,constraints,keep,inherited)
	| Ast.Exp(exp) -> Ast.Exp(expression exp)
	| Ast.TopExp(exp) -> Ast.TopExp(expression exp)
	| Ast.Ty(ty) -> Ast.Ty(fullType ty)
	| Ast.TopId(id) -> Ast.TopId(ident id)
	| Ast.TopInit(init) -> Ast.TopInit(initialiser init)
	| Ast.Include(inc,name) ->
	    let linc = string_mcode inc in
	    let lname = inc_file_mcode name in
	    Ast.Include(linc, lname)
	| Ast.MetaInclude(inc,name) ->
	    let linc = string_mcode inc in
	    let lname = expression name in
	    Ast.MetaInclude(linc, lname)
	| Ast.Undef(def,id) ->
	    let ldef = string_mcode def in
	    let lid = ident id in
	    Ast.Undef(ldef, lid)
	| Ast.DefineHeader(def,id,params) ->
	    let ldef = string_mcode def in
	    let lid = ident id in
	    let lparams = define_parameters params in
	    Ast.DefineHeader(ldef, lid, lparams)
	| Ast.Pragma(prg,id,body) ->
	    let lprg = string_mcode prg in
	    let lid = ident id in
	    let lbody = pragmainfo body in
	    Ast.Pragma(lprg, lid, lbody)
	| Ast.Default(def,colon) ->
	    let ldef = string_mcode def in
	    let lcolon = string_mcode colon in
	    Ast.Default(ldef, lcolon)
	| Ast.Case(case,exp,colon) ->
	    let lcase = string_mcode case in
	    let lexp = expression exp in
	    let lcolon = string_mcode colon in
	    Ast.Case(lcase, lexp, lcolon)
	| Ast.AsRe(re,asre) ->
	    let re = rule_elem re in
	    let asre = rule_elem asre in
	    Ast.AsRe(re,asre)
	| Ast.DisjRuleElem(res) -> Ast.DisjRuleElem(List.map rule_elem res)) in
    rulefn all_functions k re

  (* not parameterizable for now... *)
  and forinfo fi =
    let k = function
      Ast.ForExp(e1,sem1) ->
	let le1 = get_option expression e1 in
	let lsem1 = string_mcode sem1 in
	Ast.ForExp(le1, lsem1)
    | Ast.ForDecl decl -> Ast.ForDecl (annotated_decl decl) in
    k fi

  (* not parameterizable for now... *)
  and pragmainfo pi =
    let k pi =
      Ast.rewrap pi
	(match Ast.unwrap pi with
	  Ast.PragmaString(s) -> Ast.PragmaString(string_mcode s)
	| Ast.PragmaDots (dots) -> Ast.PragmaDots(string_mcode dots)) in
    k pi

  (* not parameterizable for now... *)
  and define_parameters p =
    let k p =
      Ast.rewrap p
	(match Ast.unwrap p with
	  Ast.NoParams -> Ast.NoParams
	| Ast.DParams(lp,params,rp) ->
	  let llp = string_mcode lp in
	  let lparams = define_param_dots params in
	  let lrp = string_mcode rp in
	  Ast.DParams(llp, lparams, lrp)) in
    k p

  and define_param_dots d =
    Ast.rewrap d (List.map define_param (Ast.unwrap d))

  and define_param p =
    let k p =
      Ast.rewrap p
	(match Ast.unwrap p with
	  Ast.DParam(id) -> Ast.DParam(ident id)
	| Ast.MetaDParamList(name,lenname_inh,constraints,keep,inherited) ->
	    Ast.MetaDParamList
	      (meta_mcode name,lenname_inh,constraints,keep,inherited)
	| Ast.DPComma(comma) -> Ast.DPComma(string_mcode comma)
	| Ast.DPdots(d) -> Ast.DPdots(string_mcode d)
	| Ast.OptDParam(dp) -> Ast.OptDParam(define_param dp)) in
    define_paramfn all_functions k p

  and process_bef_aft s =
    Ast.set_dots_bef_aft
      (match Ast.get_dots_bef_aft s with
	Ast.NoDots -> Ast.NoDots
      | Ast.DroppingBetweenDots(stm,ind) ->
	  Ast.DroppingBetweenDots(statement stm,ind)
      | Ast.AddingBetweenDots(stm,ind) ->
	  Ast.AddingBetweenDots(statement stm,ind))
      s

  and statement s =
    let k s =
      Ast.rewrap s
	(match Ast.unwrap s with
	  Ast.Seq(lbrace,body,rbrace) ->
	    let llbrace = rule_elem lbrace in
	    let lbody = statement_dots body in
	    let lrbrace = rule_elem rbrace in
	    Ast.Seq(llbrace, lbody, lrbrace)
	| Ast.IfThen(header,branch,aft) ->
	    let lheader = rule_elem header in
	    let lbranch = statement branch in
	    Ast.IfThen(lheader, lbranch, aft)
	| Ast.IfThenElse(header,branch1,els,branch2,aft) ->
	    let lheader = rule_elem header in
	    let lbranch1 = statement branch1 in
	    let lels = rule_elem els in
	    let lbranch2 = statement branch2 in
	    Ast.IfThenElse(lheader, lbranch1, lels, lbranch2, aft)
	| Ast.While(header,body,aft) ->
	    let lheader = rule_elem header in
	    let lbody = statement body in
	    Ast.While(lheader, lbody, aft)
	| Ast.Do(header,body,tail) ->
	    let lheader = rule_elem header in
	    let lbody = statement body in
	    let ltail = rule_elem tail in
	    Ast.Do(lheader, lbody, ltail)
	| Ast.For(header,body,aft) ->
	    let lheader = rule_elem header in
	    let lbody = statement body in
	    Ast.For(lheader, lbody, aft)
	| Ast.Iterator(header,body,aft) ->
	    let lheader = rule_elem header in
	    let lbody = statement body in
	    Ast.Iterator(lheader, lbody, aft)
	| Ast.Switch(header,lb,decls,cases,rb) ->
	    let lheader = rule_elem header in
	    let llb = rule_elem lb in
	    let ldecls = statement_dots decls in
	    let lcases = List.map case_line cases in
	    let lrb = rule_elem rb in
	    Ast.Switch(lheader, llb, ldecls, lcases, lrb)
	| Ast.Atomic(re) -> Ast.Atomic(rule_elem re)
	| Ast.Disj(stmt_dots_list) ->
	    Ast.Disj (List.map statement_dots stmt_dots_list)
	| Ast.Conj(stmt_dots_list) ->
	    Ast.Conj (List.map statement_dots stmt_dots_list)
	| Ast.Nest(starter,stmt_dots,ender,whn,multi,bef,aft) ->
	    let lstarter = string_mcode starter in
	    let lstmt_dots = statement_dots stmt_dots in
	    let lender = string_mcode ender in
	    let lwhn = List.map (whencode statement_dots statement) whn in
	    Ast.Nest(lstarter, lstmt_dots, lender, lwhn, multi, bef, aft)
	| Ast.FunDecl(header,lbrace,body,rbrace,aft) ->
	    let lheader = rule_elem header in
	    let lbraces = rule_elem lbrace in
	    let lbody = statement_dots body in
	    let lrbrace = rule_elem rbrace in
	    Ast.FunDecl(lheader, lbraces, lbody, lrbrace, aft)
	| Ast.Define(header,body) ->
	    let lheader = rule_elem header in
	    let lbody = statement_dots body in
	    Ast.Define(lheader, lbody)
	| Ast.AsStmt(stm,asstm) ->
	    let lstm = statement stm in
	    let lasstm = statement asstm in
	    Ast.AsStmt(lstm, lasstm)
	| Ast.Dots(d,whn,bef,aft) ->
	    let ld = string_mcode d in
	    let lwhn = List.map (whencode statement_dots statement) whn in
	    Ast.Dots(ld, lwhn, bef, aft)
	| Ast.OptStm(stmt) -> Ast.OptStm(statement stmt)) in
    let s = stmtfn all_functions k s in
    (* better to do this after, in case there is an equality test on the whole
       statement, eg in free_vars.  equality test would require that this
       subterm not already be changed *)
    process_bef_aft s

  and fninfo = function
      Ast.FStorage(stg) -> Ast.FStorage(storage_mcode stg)
    | Ast.FType(ty) -> Ast.FType(fullType ty)
    | Ast.FInline(inline) -> Ast.FInline(string_mcode inline)
    | Ast.FAttr(attr) -> Ast.FAttr(attribute attr)

  and attribute a =
    let k a =
      Ast.rewrap a
        (match Ast.unwrap a with
          Ast.Attribute(arg) -> Ast.Attribute(attr_arg arg)) in
            attributefn all_functions k a

  and attr_arg a =
    let k a =
      Ast.rewrap a
        (match Ast.unwrap a with
          Ast.AttrName(arg) -> Ast.AttrName(string_mcode arg)
        | Ast.MetaAttr(name,constraints,keep,inherited) ->
            Ast.MetaAttr(meta_mcode name,constraints,keep,inherited)) in
              attr_argfn all_functions k a

  and whencode notfn alwaysfn = function
      Ast.WhenNot a -> Ast.WhenNot (notfn a)
    | Ast.WhenAlways a -> Ast.WhenAlways (alwaysfn a)
    | Ast.WhenModifier(x)    -> Ast.WhenModifier(x)
    | Ast.WhenNotTrue(e) -> Ast.WhenNotTrue(rule_elem e)
    | Ast.WhenNotFalse(e) -> Ast.WhenNotFalse(rule_elem e)

  and case_line c =
    let k c =
      Ast.rewrap c
	(match Ast.unwrap c with
	  Ast.CaseLine(header,code) ->
	    let lheader = rule_elem header in
	    let lcode = statement_dots code in
	    Ast.CaseLine(lheader, lcode)
	| Ast.OptCase(case) -> Ast.OptCase(case_line case)) in
    casefn all_functions k c

  and exec_code e =
    (* not configurable *)
    Ast.rewrap e
      (match Ast.unwrap e with
	Ast.ExecEval(colon,id) ->
	  let lcolon = string_mcode colon in
	  let lid = expression id in
	  Ast.ExecEval(lcolon, lid)
      | Ast.ExecToken(tok) -> Ast.ExecToken(string_mcode tok)
      | Ast.ExecDots(dots) -> Ast.ExecDots(string_mcode dots))

  and top_level t =
    let k t =
      Ast.rewrap t
	(match Ast.unwrap t with
	  Ast.FILEINFO(old_file,new_file) ->
	    Ast.FILEINFO (string_mcode old_file, string_mcode new_file)
	| Ast.NONDECL(stmt) -> Ast.NONDECL(statement stmt)
	| Ast.CODE(stmt_dots) -> Ast.CODE(statement_dots stmt_dots)
	| Ast.ERRORWORDS(exps) -> Ast.ERRORWORDS (List.map expression exps)) in
    topfn all_functions k t

  and anything a =
    let k = function
	(*in many cases below, the thing is not even mcode, so we do nothing*)
	Ast.FullTypeTag(ft) -> Ast.FullTypeTag(fullType ft)
      | Ast.BaseTypeTag(bt) as x -> x
      | Ast.StructUnionTag(su) as x -> x
      | Ast.SignTag(sgn) as x -> x
      | Ast.IdentTag(id) -> Ast.IdentTag(ident id)
      | Ast.ExpressionTag(exp) -> Ast.ExpressionTag(expression exp)
      | Ast.ConstantTag(cst) as x -> x
      | Ast.UnaryOpTag(unop) as x -> x
      | Ast.AssignOpTag(asgnop) as x -> x
      | Ast.SimpleAssignOpTag _ as x -> x
      | Ast.OpAssignOpTag _ as x -> x
      | Ast.FixOpTag(fixop) as x -> x
      | Ast.BinaryOpTag(binop) as x -> x
      | Ast.ArithOpTag(arithop) as x -> x
      | Ast.LogicalOpTag(logop) as x -> x
      | Ast.InitTag(decl) -> Ast.InitTag(initialiser decl)
      | Ast.DeclarationTag(decl) -> Ast.DeclarationTag(declaration decl)
      | Ast.FieldTag(decl) -> Ast.FieldTag(field decl)
      | Ast.EnumDeclTag(decl) -> Ast.EnumDeclTag(enum_decl decl)
      | Ast.StorageTag(stg) as x -> x
      | Ast.IncFileTag(stg) as x -> x
      | Ast.Rule_elemTag(rule) -> Ast.Rule_elemTag(rule_elem rule)
      | Ast.StatementTag(rule) -> Ast.StatementTag(statement rule)
      | Ast.ForInfoTag(rule) -> Ast.ForInfoTag(forinfo rule)
      | Ast.CaseLineTag(case) -> Ast.CaseLineTag(case_line case)
      | Ast.StringFragmentTag(frag) ->
	  Ast.StringFragmentTag(string_fragment frag)
      | Ast.AttributeTag(attr) -> Ast.AttributeTag(attribute attr)
      | Ast.AttrArgTag(arg) -> Ast.AttrArgTag(attr_arg arg)
      | Ast.ConstVolTag(cv) as x -> x
      | Ast.Token(tok,info) as x -> x
      | Ast.Directive(str) as x -> x
      | Ast.Code(cd) -> Ast.Code(top_level cd)
      | Ast.ExprDotsTag(ed) -> Ast.ExprDotsTag(expression_dots ed)
      | Ast.ParamDotsTag(pd) -> Ast.ParamDotsTag(parameter_dots pd)
      | Ast.StmtDotsTag(sd) -> Ast.StmtDotsTag(statement_dots sd)
      | Ast.AnnDeclDotsTag(sd) -> Ast.AnnDeclDotsTag(annotated_decl_dots sd)
      | Ast.AnnFieldDotsTag(sd) -> Ast.AnnFieldDotsTag(annotated_field_dots sd)
      | Ast.EnumDeclDotsTag(sd) -> Ast.EnumDeclDotsTag(enum_decl_dots sd)
      | Ast.DefParDotsTag(sd) -> Ast.DefParDotsTag(define_param_dots sd)
      | Ast.TypeCTag(ty) -> Ast.TypeCTag(typeC ty)
      | Ast.ParamTag(param) -> Ast.ParamTag(parameterTypeDef param)
      | Ast.SgrepStartTag(tok) as x -> x
      | Ast.SgrepEndTag(tok) as x -> x in
    anyfn all_functions k a

  and all_functions =
    {rebuilder_ident = ident;
      rebuilder_expression = expression;
      rebuilder_fragment = string_fragment;
      rebuilder_format = string_format;
      rebuilder_assignOp = assignOp;
      rebuilder_binaryOp = binaryOp;
      rebuilder_fullType = fullType;
      rebuilder_typeC = typeC;
      rebuilder_declaration = declaration;
      rebuilder_field = field;
      rebuilder_ann_field = annotated_field;
      rebuilder_enumdecl = enum_decl;
      rebuilder_initialiser = initialiser;
      rebuilder_parameter = parameterTypeDef;
      rebuilder_parameter_list = parameter_dots;
      rebuilder_rule_elem = rule_elem;
      rebuilder_statement = statement;
      rebuilder_case_line = case_line;
      rebuilder_attribute = attribute;
      rebuilder_attr_arg = attr_arg;
      rebuilder_top_level = top_level;
      rebuilder_expression_dots = expression_dots;
      rebuilder_statement_dots = statement_dots;
      rebuilder_anndecl_dots = annotated_decl_dots;
      rebuilder_annfield_dots = annotated_field_dots;
      rebuilder_enumdecl_dots = enum_decl_dots;
      rebuilder_initialiser_dots = initialiser_dots;
      rebuilder_define_param_dots = define_param_dots;
      rebuilder_define_param = define_param;
      rebuilder_define_parameters = define_parameters;
      rebuilder_anything = anything} in
  all_functions
