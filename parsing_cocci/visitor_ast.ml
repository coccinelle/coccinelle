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


# 0 "./visitor_ast.ml"
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
     combiner_fullType : Ast.fullType -> 'a;
     combiner_typeC : Ast.typeC -> 'a;
     combiner_declaration : Ast.declaration -> 'a;
     combiner_initialiser : Ast.initialiser -> 'a;
     combiner_parameter : Ast.parameterTypeDef -> 'a;
     combiner_parameter_list : Ast.parameter_list -> 'a;
     combiner_rule_elem : Ast.rule_elem -> 'a;
     combiner_statement : Ast.statement -> 'a;
     combiner_case_line : Ast.case_line -> 'a;
     combiner_top_level : Ast.top_level -> 'a;
     combiner_anything : Ast.anything  -> 'a;
     combiner_expression_dots : Ast.expression Ast.dots -> 'a;
     combiner_statement_dots : Ast.statement Ast.dots -> 'a;
     combiner_declaration_dots : Ast.declaration Ast.dots -> 'a;
     combiner_initialiser_dots : Ast.initialiser Ast.dots -> 'a}

type ('mc,'a) cmcode = 'a combiner -> 'mc Ast_cocci.mcode -> 'a
type ('cd,'a) ccode = 'a combiner -> ('cd -> 'a) -> 'cd -> 'a


let combiner bind option_default
    meta_mcodefn string_mcodefn const_mcodefn assign_mcodefn fix_mcodefn
    unary_mcodefn binary_mcodefn
    cv_mcodefn sign_mcodefn struct_mcodefn storage_mcodefn
    inc_file_mcodefn
    expdotsfn paramdotsfn stmtdotsfn decldotsfn initdotsfn
    identfn exprfn fragfn fmtfn ftfn tyfn initfn paramfn declfn rulefn
    stmtfn casefn topfn anyfn =
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
    let k d =
      match Ast.unwrap d with
	Ast.DOTS(l) | Ast.CIRCLES(l) | Ast.STARS(l) ->
	  multibind (List.map default l) in
    param all_functions k arg in

  let rec meta_mcode x = meta_mcodefn all_functions x
  and string_mcode x = string_mcodefn all_functions x
  and const_mcode x = const_mcodefn all_functions x
  and assign_mcode x = assign_mcodefn all_functions x
  and fix_mcode x = fix_mcodefn all_functions x
  and unary_mcode x = unary_mcodefn all_functions x
  and binary_mcode x = binary_mcodefn all_functions x
  and cv_mcode x = cv_mcodefn all_functions x
  and sign_mcode x = sign_mcodefn all_functions x
  and struct_mcode x = struct_mcodefn all_functions x
  and storage_mcode x = storage_mcodefn all_functions x
  and inc_file_mcode x = inc_file_mcodefn all_functions x

  and iddotsfn all_functions k arg = k arg
  and strdotsfn all_functions k arg = k arg
  and ecdotsfn all_functions k arg = k arg

  and expression_dots d = dotsfn expdotsfn expression all_functions d
  and identifier_dots d = dotsfn iddotsfn ident all_functions d
  and parameter_dots d = dotsfn paramdotsfn parameterTypeDef all_functions d
  and statement_dots d = dotsfn stmtdotsfn statement all_functions d
  and declaration_dots d = dotsfn decldotsfn declaration all_functions d
  and initialiser_dots d = dotsfn initdotsfn initialiser all_functions d
  and string_fragment_dots d = dotsfn strdotsfn string_fragment all_functions d
  and exec_code_dots d = dotsfn ecdotsfn exec_code all_functions d

  and ident i =
    let k i =
      match Ast.unwrap i with
	Ast.Id(name) -> string_mcode name
      | Ast.MetaId(name,_,_,_) -> meta_mcode name
      | Ast.MetaFunc(name,_,_,_) -> meta_mcode name
      | Ast.MetaLocalFunc(name,_,_,_) -> meta_mcode name
      |	Ast.AsIdent(id,asid) -> bind (ident id) (ident asid)
      | Ast.DisjId(id_list) -> multibind (List.map ident id_list)
      | Ast.OptIdent(id) -> ident id
      | Ast.UniqueIdent(id) -> ident id in
    identfn all_functions k i

  and expression e =
    let k e =
      match Ast.unwrap e with
	Ast.Ident(id) -> ident id
      | Ast.Constant(const) -> const_mcode const
      | Ast.StringConstant(lq,str,rq) ->
	  multibind
	    [string_mcode lq; string_fragment_dots str; string_mcode rq]
      | Ast.FunCall(fn,lp,args,rp) ->
	  multibind [expression fn; string_mcode lp; expression_dots args;
		      string_mcode rp]
      | Ast.Assignment(left,op,right,simple) ->
	  multibind [expression left; assign_mcode op; expression right]
      | Ast.Sequence(left,op,right) ->
	  multibind [expression left; string_mcode op; expression right]
      | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
	  multibind [expression exp1; string_mcode why;
		      get_option expression exp2; string_mcode colon;
		      expression exp3]
      | Ast.Postfix(exp,op) -> bind (expression exp) (fix_mcode op)
      | Ast.Infix(exp,op) -> bind (fix_mcode op) (expression exp)
      | Ast.Unary(exp,op) -> bind (unary_mcode op) (expression exp)
      | Ast.Binary(left,op,right) ->
	  multibind [expression left; binary_mcode op; expression right]
      | Ast.Nested(left,op,right) ->
	  multibind [expression left; binary_mcode op; expression right]
      | Ast.Paren(lp,exp,rp) ->
	  multibind [string_mcode lp; expression exp; string_mcode rp]
      | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
	  multibind
	    [expression exp1; string_mcode lb; expression exp2;
	      string_mcode rb]
      | Ast.RecordAccess(exp,pt,field) ->
	  multibind [expression exp; string_mcode pt; ident field]
      | Ast.RecordPtAccess(exp,ar,field) ->
	  multibind [expression exp; string_mcode ar; ident field]
      | Ast.Cast(lp,ty,rp,exp) ->
	  multibind
	    [string_mcode lp; fullType ty; string_mcode rp; expression exp]
      | Ast.SizeOfExpr(szf,exp) ->
	  multibind [string_mcode szf; expression exp]
      | Ast.SizeOfType(szf,lp,ty,rp) ->
	  multibind
	    [string_mcode szf; string_mcode lp; fullType ty; string_mcode rp]
      | Ast.TypeExp(ty) -> fullType ty
      | Ast.Constructor(lp,ty,rp,init) ->
	  multibind
	    [string_mcode lp; fullType ty; string_mcode rp; initialiser init]
      | Ast.MetaErr(name,_,_,_)
      | Ast.MetaExpr(name,_,_,_,_,_)
      | Ast.MetaExprList(name,_,_,_) -> meta_mcode name
      |	Ast.AsExpr(exp,asexp) -> bind (expression exp) (expression asexp)
      | Ast.EComma(cm) -> string_mcode cm
      | Ast.DisjExpr(exp_list) -> multibind (List.map expression exp_list)
      | Ast.NestExpr(starter,expr_dots,ender,whencode,multi) ->
	  bind (string_mcode starter)
	    (bind (expression_dots expr_dots)
	       (bind (string_mcode ender)
		  (get_option expression whencode)))
      | Ast.Edots(dots,whencode) | Ast.Ecircles(dots,whencode)
      | Ast.Estars(dots,whencode) ->
	  bind (string_mcode dots) (get_option expression whencode)
      | Ast.OptExp(exp) | Ast.UniqueExp(exp) ->
	  expression exp in
    exprfn all_functions k e

  and string_fragment e =
    let k e =
      match Ast.unwrap e with
	Ast.ConstantFragment(str) -> string_mcode str
      | Ast.FormatFragment(pct,fmt) ->
	  let pct = string_mcode pct in
	  let fmt = string_format fmt in
	  bind pct fmt
      |	Ast.Strdots dots -> string_mcode dots
      | Ast.MetaFormatList(pct,name,lenname,_,_) ->
	  let pct = string_mcode pct in
	  let name = meta_mcode name in
	  multibind [pct;name] in
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
	Ast.Type(_,cv,ty) -> bind (get_option cv_mcode cv) (typeC ty)
      |	Ast.AsType(ty,asty) -> bind (fullType ty) (fullType asty)
      | Ast.DisjType(types) -> multibind (List.map fullType types)
      | Ast.OptType(ty) -> fullType ty
      | Ast.UniqueType(ty) -> fullType ty in
    ftfn all_functions k ft

  and function_pointer (ty,lp1,star,rp1,lp2,params,rp2) extra =
    (* have to put the treatment of the identifier into the right position *)
    multibind
      ([fullType ty; string_mcode lp1; string_mcode star] @ extra @
       [string_mcode rp1;
	 string_mcode lp2; parameter_dots params; string_mcode rp2])

  and function_type (ty,lp1,params,rp1) extra =
    (* have to put the treatment of the identifier into the right position *)
    multibind
      ([get_option fullType ty] @ extra @
       [string_mcode lp1; parameter_dots params; string_mcode rp1])

  and array_type (ty,lb,size,rb) extra =
    multibind
      ([fullType ty] @ extra @
       [string_mcode lb; get_option expression size; string_mcode rb])

  and typeC ty =
    let k ty =
      match Ast.unwrap ty with
	Ast.BaseType(ty,strings) -> multibind (List.map string_mcode strings)
      | Ast.SignedT(sgn,ty) -> bind (sign_mcode sgn) (get_option typeC ty)
      | Ast.Pointer(ty,star) ->
	  bind (fullType ty) (string_mcode star)
      | Ast.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
	  function_pointer (ty,lp1,star,rp1,lp2,params,rp2) []
      |	Ast.FunctionType (_,ty,lp1,params,rp1) ->
	  function_type (ty,lp1,params,rp1) []
      | Ast.Array(ty,lb,size,rb) -> array_type (ty,lb,size,rb) []
      | Ast.Decimal(dec,lp,length,comma,precision_opt,rp) ->
	  multibind
	    [string_mcode dec; string_mcode lp;
	      expression length; get_option string_mcode comma;
	      get_option expression precision_opt; string_mcode rp]
      | Ast.EnumName(kind,name) ->
	  bind (string_mcode kind) (get_option ident name)
      | Ast.EnumDef(ty,lb,ids,rb) ->
	  multibind
	    [fullType ty; string_mcode lb; expression_dots ids;
	      string_mcode rb]
      | Ast.StructUnionName(kind,name) ->
	  bind (struct_mcode kind) (get_option ident name)
      | Ast.StructUnionDef(ty,lb,decls,rb) ->
	  multibind
	    [fullType ty; string_mcode lb; declaration_dots decls;
	      string_mcode rb]
      | Ast.TypeName(name) -> string_mcode name
      | Ast.MetaType(name,_,_) -> meta_mcode name in
    tyfn all_functions k ty

  and named_type ty id =
    match Ast.unwrap ty with
      Ast.Type(_,None,ty1) ->
	(match Ast.unwrap ty1 with
	  Ast.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
	    function_pointer (ty,lp1,star,rp1,lp2,params,rp2) [ident id]
	| Ast.FunctionType(_,ty,lp1,params,rp1) ->
	    function_type (ty,lp1,params,rp1) [ident id]
	| Ast.Array(ty,lb,size,rb) -> array_type (ty,lb,size,rb) [ident id]
	| _ -> bind (fullType ty) (ident id))
    | _ -> bind (fullType ty) (ident id)

  and declaration d =
    let k d =
      match Ast.unwrap d with
	Ast.MetaDecl(name,_,_) | Ast.MetaField(name,_,_)
      |	Ast.MetaFieldList(name,_,_,_) ->
	  meta_mcode name
      |	Ast.AsDecl(decl,asdecl) ->
	  bind (declaration decl) (declaration asdecl)
      |	Ast.Init(stg,ty,id,eq,ini,sem) ->
	  bind (get_option storage_mcode stg)
	    (bind (named_type ty id)
	       (multibind
		  [string_mcode eq; initialiser ini; string_mcode sem]))
      | Ast.UnInit(stg,ty,id,sem) ->
	  bind (get_option storage_mcode stg)
	    (bind (named_type ty id) (string_mcode sem))
      | Ast.MacroDecl(name,lp,args,rp,sem) ->
	  multibind
	    [ident name; string_mcode lp; expression_dots args;
	      string_mcode rp; string_mcode sem]
      | Ast.MacroDeclInit(name,lp,args,rp,eq,ini,sem) ->
	  multibind
	    [ident name; string_mcode lp; expression_dots args;
	      string_mcode rp; string_mcode eq; initialiser ini;
	      string_mcode sem]
      | Ast.TyDecl(ty,sem) -> bind (fullType ty) (string_mcode sem)
      | Ast.Typedef(stg,ty,id,sem) ->
	  bind (string_mcode stg)
	    (bind (fullType ty) (bind (typeC id) (string_mcode sem)))
      | Ast.DisjDecl(decls) -> multibind (List.map declaration decls)
      |	Ast.Ddots(dots,whencode) ->
	  bind (string_mcode dots) (get_option declaration whencode)
      | Ast.OptDecl(decl) -> declaration decl
      | Ast.UniqueDecl(decl) -> declaration decl in
    declfn all_functions k d

  and initialiser i =
    let k i =
      match Ast.unwrap i with
	Ast.MetaInit(name,_,_) -> meta_mcode name
      |	Ast.MetaInitList(name,_,_,_) -> meta_mcode name
      |	Ast.AsInit(init,asinit) ->
	  bind (initialiser init) (initialiser asinit)
      |	Ast.InitExpr(exp) -> expression exp
      | Ast.ArInitList(lb,initlist,rb) ->
	  multibind
	    [string_mcode lb; initialiser_dots initlist; string_mcode rb]
      | Ast.StrInitList(allminus,lb,initlist,rb,whencode) ->
	  multibind
	    [string_mcode lb;
	      multibind (List.map initialiser initlist);
	      string_mcode rb;
	      multibind (List.map initialiser whencode)]
      | Ast.InitGccName(name,eq,ini) ->
	  multibind [ident name; string_mcode eq; initialiser ini]
      | Ast.InitGccExt(designators,eq,ini) ->
	  multibind
	    ((List.map designator designators) @
	     [string_mcode eq; initialiser ini])
      | Ast.IComma(cm) -> string_mcode cm
      | Ast.Idots(dots,whencode) ->
	  bind (string_mcode dots) (get_option initialiser whencode)
      | Ast.OptIni(i) -> initialiser i
      | Ast.UniqueIni(i) -> initialiser i in
    initfn all_functions k i

  and designator = function
      Ast.DesignatorField(dot,id) -> bind (string_mcode dot) (ident id)
    | Ast.DesignatorIndex(lb,exp,rb) ->
	bind (string_mcode lb) (bind (expression exp) (string_mcode rb))
    | Ast.DesignatorRange(lb,min,dots,max,rb) ->
	multibind
	  [string_mcode lb; expression min; string_mcode dots;
	    expression max; string_mcode rb]

  and parameterTypeDef p =
    let k p =
      match Ast.unwrap p with
	Ast.VoidParam(ty) -> fullType ty
      | Ast.Param(ty,Some id) -> named_type ty id
      | Ast.Param(ty,None) -> fullType ty
      | Ast.MetaParam(name,_,_) -> meta_mcode name
      | Ast.MetaParamList(name,_,_,_) -> meta_mcode name
      |	Ast.AsParam(p,asexp) -> bind (parameterTypeDef p) (expression asexp)
      | Ast.PComma(cm) -> string_mcode cm
      | Ast.Pdots(dots) -> string_mcode dots
      | Ast.Pcircles(dots) -> string_mcode dots
      | Ast.OptParam(param) -> parameterTypeDef param
      | Ast.UniqueParam(param) -> parameterTypeDef param in
    paramfn all_functions k p

  and rule_elem re =
    let k re =
      match Ast.unwrap re with
	Ast.FunHeader(_,_,fi,name,lp,params,rp) ->
	  multibind
	    ((List.map fninfo fi) @
	     [ident name;string_mcode lp;parameter_dots params;
	       string_mcode rp])
      | Ast.Decl(_,_,decl) -> declaration decl
      | Ast.SeqStart(brace) -> string_mcode brace
      | Ast.SeqEnd(brace) -> string_mcode brace
      | Ast.ExprStatement(exp,sem) ->
	  bind (get_option expression exp) (string_mcode sem)
      | Ast.IfHeader(iff,lp,exp,rp) ->
	  multibind [string_mcode iff; string_mcode lp; expression exp;
		      string_mcode rp]
      | Ast.Else(els) -> string_mcode els
      | Ast.WhileHeader(whl,lp,exp,rp) ->
	  multibind [string_mcode whl; string_mcode lp; expression exp;
		      string_mcode rp]
      | Ast.DoHeader(d) -> string_mcode d
      | Ast.WhileTail(whl,lp,exp,rp,sem) ->
	  multibind [string_mcode whl; string_mcode lp; expression exp;
		      string_mcode rp; string_mcode sem]
      | Ast.ForHeader(fr,lp,first,e2,sem2,e3,rp) ->
	  let first = forinfo first in
	  multibind [string_mcode fr; string_mcode lp; first;
		      get_option expression e2; string_mcode sem2;
		      get_option expression e3; string_mcode rp]
      | Ast.IteratorHeader(nm,lp,args,rp) ->
	  multibind [ident nm; string_mcode lp;
		      expression_dots args; string_mcode rp]
      | Ast.SwitchHeader(switch,lp,exp,rp) ->
	  multibind [string_mcode switch; string_mcode lp; expression exp;
		      string_mcode rp]
      | Ast.Break(br,sem) -> bind (string_mcode br) (string_mcode sem)
      | Ast.Continue(cont,sem) -> bind (string_mcode cont) (string_mcode sem)
      |	Ast.Label(l,dd) -> bind (ident l) (string_mcode dd)
      |	Ast.Goto(goto,l,sem) ->
	  bind (string_mcode goto) (bind (ident l) (string_mcode sem))
      | Ast.Return(ret,sem) -> bind (string_mcode ret) (string_mcode sem)
      | Ast.ReturnExpr(ret,exp,sem) ->
	  multibind [string_mcode ret; expression exp; string_mcode sem]
      |	Ast.Exec(exec,lang,code,sem) ->
	  multibind
	    [string_mcode exec; string_mcode lang; exec_code_dots code;
	      string_mcode sem]
      | Ast.MetaStmt(name,_,_,_) -> meta_mcode name
      | Ast.MetaStmtList(name,_,_) -> meta_mcode name
      | Ast.MetaRuleElem(name,_,_) -> meta_mcode name
      | Ast.Exp(exp) -> expression exp
      | Ast.TopExp(exp) -> expression exp
      | Ast.Ty(ty) -> fullType ty
      | Ast.TopInit(init) -> initialiser init
      |	Ast.Include(inc,name) -> bind (string_mcode inc) (inc_file_mcode name)
      |	Ast.Undef(def,id) ->
	  multibind [string_mcode def; ident id]
      |	Ast.DefineHeader(def,id,params) ->
	  multibind [string_mcode def; ident id; define_parameters params]
      |	Ast.Pragma(prg,id,body) ->
	  multibind [string_mcode prg; ident id; pragmainfo body]
      |	Ast.Default(def,colon) -> bind (string_mcode def) (string_mcode colon)
      |	Ast.Case(case,exp,colon) ->
	  multibind [string_mcode case; expression exp; string_mcode colon]
      |	Ast.DisjRuleElem(res) -> multibind (List.map rule_elem res) in
    rulefn all_functions k re

  (* not parameterisable, for now *)
  and forinfo fi =
    let k = function
	Ast.ForExp(e1,sem1) ->
	  bind (get_option expression e1) (string_mcode sem1)
      | Ast.ForDecl (_,_,decl) -> declaration decl in
    k fi

  (* not parameterisable, for now *)
  and pragmainfo pi =
    let k pi =
      match Ast.unwrap pi with
	Ast.PragmaTuple(lp,args,rp) ->
	  multibind [string_mcode lp;expression_dots args;string_mcode rp]
      | Ast.PragmaIdList(ids) -> identifier_dots ids
      | Ast.PragmaDots (dots) -> string_mcode dots in
    k pi

  (* not parameterizable for now... *)
  and define_parameters p =
    let k p =
      match Ast.unwrap p with
	Ast.NoParams -> option_default
      | Ast.DParams(lp,params,rp) ->
	  multibind
	    [string_mcode lp; define_param_dots params; string_mcode rp] in
    k p

  and define_param_dots d =
    let k d =
      match Ast.unwrap d with
	Ast.DOTS(l) | Ast.CIRCLES(l) | Ast.STARS(l) ->
	  multibind (List.map define_param l) in
    k d

  and define_param p =
    let k p =
      match Ast.unwrap p with
	Ast.DParam(id) -> ident id
      | Ast.DPComma(comma) -> string_mcode comma
      | Ast.DPdots(d) -> string_mcode d
      | Ast.DPcircles(c) -> string_mcode c
      | Ast.OptDParam(dp) -> define_param dp
      | Ast.UniqueDParam(dp) -> define_param dp in
    k p

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
	  multibind [rule_elem lbrace;
		      statement_dots body; rule_elem rbrace]
      | Ast.IfThen(header,branch,_) ->
	  multibind [rule_elem header; statement branch]
      | Ast.IfThenElse(header,branch1,els,branch2,_) ->
	  multibind [rule_elem header; statement branch1; rule_elem els;
		      statement branch2]
      | Ast.While(header,body,_) ->
	  multibind [rule_elem header; statement body]
      | Ast.Do(header,body,tail) ->
	  multibind [rule_elem header; statement body; rule_elem tail]
      | Ast.For(header,body,_) -> multibind [rule_elem header; statement body]
      | Ast.Iterator(header,body,_) ->
	  multibind [rule_elem header; statement body]
      |	Ast.Switch(header,lb,decls,cases,rb) ->
	  multibind [rule_elem header;rule_elem lb;
		      statement_dots decls;
		      multibind (List.map case_line cases);
		      rule_elem rb]
      | Ast.Atomic(re) -> rule_elem re
      | Ast.Disj(stmt_dots_list) ->
	  multibind (List.map statement_dots stmt_dots_list)
      | Ast.Nest(starter,stmt_dots,ender,whn,_,_,_) ->
	  bind (string_mcode starter)
	    (bind (statement_dots stmt_dots)
	       (bind (string_mcode ender)
		  (multibind
		     (List.map (whencode statement_dots statement) whn))))
      | Ast.FunDecl(header,lbrace,body,rbrace) ->
	  multibind [rule_elem header; rule_elem lbrace;
		      statement_dots body; rule_elem rbrace]
      | Ast.Define(header,body) ->
	  bind (rule_elem header) (statement_dots body)
      |	Ast.AsStmt(stm,asstm) ->
	  bind (statement stm) (statement asstm)
      | Ast.Dots(d,whn,_,_) | Ast.Circles(d,whn,_,_) | Ast.Stars(d,whn,_,_) ->
	  bind (string_mcode d)
	    (multibind (List.map (whencode statement_dots statement) whn))
      | Ast.OptStm(stmt) | Ast.UniqueStm(stmt) ->
	  statement stmt in
    stmtfn all_functions k s

  and fninfo = function
      Ast.FStorage(stg) -> storage_mcode stg
    | Ast.FType(ty) -> fullType ty
    | Ast.FInline(inline) -> string_mcode inline
    | Ast.FAttr(attr) -> string_mcode attr

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
	  bind (rule_elem header) (statement_dots code)
      |	Ast.OptCase(case) -> case_line case in
    casefn all_functions k c

  and exec_code e =
    (* not configurable *)
    match Ast.unwrap e with
      Ast.ExecEval(colon,id) -> bind (string_mcode colon) (expression id)
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
      | Ast.AssignOpTag(asgnop) -> option_default
      | Ast.FixOpTag(fixop) -> option_default
      | Ast.BinaryOpTag(binop) -> option_default
      | Ast.ArithOpTag(arithop) -> option_default
      | Ast.LogicalOpTag(logop) -> option_default
      | Ast.DeclarationTag(decl) -> declaration decl
      | Ast.InitTag(ini) -> initialiser ini
      | Ast.StorageTag(stg) -> option_default
      | Ast.IncFileTag(stg) -> option_default
      | Ast.Rule_elemTag(rule) -> rule_elem rule
      | Ast.StatementTag(rule) -> statement rule
      | Ast.ForInfoTag(rule) -> forinfo rule
      | Ast.CaseLineTag(case) -> case_line case
      | Ast.StringFragmentTag(frag) -> string_fragment frag
      | Ast.ConstVolTag(cv) -> option_default
      | Ast.Token(tok,info) -> option_default
      | Ast.Directive(str) -> option_default
      | Ast.Code(cd) -> top_level cd
      | Ast.ExprDotsTag(ed) -> expression_dots ed
      | Ast.ParamDotsTag(pd) -> parameter_dots pd
      | Ast.StmtDotsTag(sd) -> statement_dots sd
      | Ast.DeclDotsTag(sd) -> declaration_dots sd
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
      combiner_fullType = fullType;
      combiner_typeC = typeC;
      combiner_declaration = declaration;
      combiner_initialiser = initialiser;
      combiner_parameter = parameterTypeDef;
      combiner_parameter_list = parameter_dots;
      combiner_rule_elem = rule_elem;
      combiner_statement = statement;
      combiner_case_line = case_line;
      combiner_top_level = top_level;
      combiner_anything = anything;
      combiner_expression_dots = expression_dots;
      combiner_statement_dots = statement_dots;
      combiner_declaration_dots = declaration_dots;
      combiner_initialiser_dots = initialiser_dots} in
  all_functions

(* ---------------------------------------------------------------------- *)

type 'a inout = 'a -> 'a (* for specifying the type of rebuilder *)

type rebuilder =
    {rebuilder_ident : Ast.ident inout;
      rebuilder_expression : Ast.expression inout;
      rebuilder_fragment : Ast.string_fragment inout;
      rebuilder_format : Ast.string_format inout;
      rebuilder_fullType : Ast.fullType inout;
      rebuilder_typeC : Ast.typeC inout;
      rebuilder_declaration : Ast.declaration inout;
      rebuilder_initialiser : Ast.initialiser inout;
      rebuilder_parameter : Ast.parameterTypeDef inout;
      rebuilder_parameter_list : Ast.parameter_list inout;
      rebuilder_statement : Ast.statement inout;
      rebuilder_case_line : Ast.case_line inout;
      rebuilder_rule_elem : Ast.rule_elem inout;
      rebuilder_top_level : Ast.top_level inout;
      rebuilder_expression_dots : Ast.expression Ast.dots inout;
      rebuilder_statement_dots : Ast.statement Ast.dots inout;
      rebuilder_declaration_dots : Ast.declaration Ast.dots inout;
      rebuilder_initialiser_dots : Ast.initialiser Ast.dots inout;
      rebuilder_define_param_dots : Ast.define_param Ast.dots inout;
      rebuilder_define_param : Ast.define_param inout;
      rebuilder_define_parameters : Ast.define_parameters inout;
      rebuilder_anything : Ast.anything inout}

type 'mc rmcode = 'mc Ast.mcode inout
type 'cd rcode = rebuilder -> ('cd inout) -> 'cd inout


let rebuilder
    meta_mcode string_mcode const_mcode assign_mcode fix_mcode unary_mcode
    binary_mcode cv_mcode sign_mcode struct_mcode storage_mcode
    inc_file_mcode
    expdotsfn paramdotsfn stmtdotsfn decldotsfn initdotsfn
    identfn exprfn fragfn fmtfn ftfn tyfn initfn paramfn declfn rulefn
    stmtfn casefn topfn anyfn =
  let get_option f = function
      Some x -> Some (f x)
    | None -> None in

  let dotsfn param default all_functions arg =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.DOTS(l) -> Ast.DOTS(List.map default l)
	| Ast.CIRCLES(l) -> Ast.CIRCLES(List.map default l)
	| Ast.STARS(l) -> Ast.STARS(List.map default l)) in
    param all_functions k arg in

  let iddotsfn all_functions k arg = k arg in
  let strdotsfn all_functions k arg = k arg in
  let ecdotsfn all_functions k arg = k arg in

  let rec expression_dots d = dotsfn expdotsfn expression all_functions d
  and identifier_dots d = dotsfn iddotsfn ident all_functions d
  and parameter_dots d = dotsfn paramdotsfn parameterTypeDef all_functions d
  and statement_dots d = dotsfn stmtdotsfn statement all_functions d
  and declaration_dots d = dotsfn decldotsfn declaration all_functions d
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
	| Ast.OptIdent(id) -> Ast.OptIdent(ident id)
	| Ast.UniqueIdent(id) -> Ast.UniqueIdent(ident id)) in
    identfn all_functions k i

  and expression e =
    let k e =
      Ast.rewrap e
	(match Ast.unwrap e with
	  Ast.Ident(id) -> Ast.Ident(ident id)
	| Ast.Constant(const) -> Ast.Constant(const_mcode const)
	| Ast.StringConstant(lq,str,rq) ->
	    Ast.StringConstant(string_mcode lq, string_fragment_dots str,
			       string_mcode rq)
	| Ast.FunCall(fn,lp,args,rp) ->
	    Ast.FunCall(expression fn, string_mcode lp, expression_dots args,
			string_mcode rp)
	| Ast.Assignment(left,op,right,simple) ->
	    Ast.Assignment(expression left, assign_mcode op, expression right,
			   simple)
	| Ast.Sequence(left,op,right) ->
	    Ast.Sequence(expression left, string_mcode op, expression right)
	| Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
	    Ast.CondExpr(expression exp1, string_mcode why,
			 get_option expression exp2, string_mcode colon,
			 expression exp3)
	| Ast.Postfix(exp,op) -> Ast.Postfix(expression exp,fix_mcode op)
	| Ast.Infix(exp,op) -> Ast.Infix(expression exp,fix_mcode op)
	| Ast.Unary(exp,op) -> Ast.Unary(expression exp,unary_mcode op)
	| Ast.Binary(left,op,right) ->
	    Ast.Binary(expression left, binary_mcode op, expression right)
	| Ast.Nested(left,op,right) ->
	    Ast.Nested(expression left, binary_mcode op, expression right)
	| Ast.Paren(lp,exp,rp) ->
	    Ast.Paren(string_mcode lp, expression exp, string_mcode rp)
	| Ast.ArrayAccess(exp1,lb,exp2,rb) ->
	    Ast.ArrayAccess(expression exp1, string_mcode lb, expression exp2,
			    string_mcode rb)
	| Ast.RecordAccess(exp,pt,field) ->
	    Ast.RecordAccess(expression exp, string_mcode pt, ident field)
	| Ast.RecordPtAccess(exp,ar,field) ->
	    Ast.RecordPtAccess(expression exp, string_mcode ar, ident field)
	| Ast.Cast(lp,ty,rp,exp) ->
	    Ast.Cast(string_mcode lp, fullType ty, string_mcode rp,
		     expression exp)
	| Ast.SizeOfExpr(szf,exp) ->
	    Ast.SizeOfExpr(string_mcode szf, expression exp)
	| Ast.SizeOfType(szf,lp,ty,rp) ->
	    Ast.SizeOfType(string_mcode szf,string_mcode lp, fullType ty,
                           string_mcode rp)
	| Ast.TypeExp(ty) -> Ast.TypeExp(fullType ty)
	| Ast.Constructor(lp,ty,rp,init) ->
	    Ast.Constructor(string_mcode lp, fullType ty, string_mcode rp,
		     initialiser init)
	| Ast.MetaErr(name,constraints,keep,inherited) ->
	    Ast.MetaErr(meta_mcode name,constraints,keep,inherited)
	| Ast.MetaExpr(name,constraints,keep,ty,form,inherited) ->
	    Ast.MetaExpr(meta_mcode name,constraints,keep,ty,form,inherited)
	| Ast.MetaExprList(name,lenname_inh,keep,inherited) ->
	    Ast.MetaExprList(meta_mcode name,lenname_inh,keep,inherited)
	| Ast.AsExpr(exp,asexp) -> Ast.AsExpr(expression exp,expression asexp)
	| Ast.EComma(cm) -> Ast.EComma(string_mcode cm)
	| Ast.DisjExpr(exp_list) -> Ast.DisjExpr(List.map expression exp_list)
	| Ast.NestExpr(starter,expr_dots,ender,whencode,multi) ->
	    Ast.NestExpr(string_mcode starter,expression_dots expr_dots,
			 string_mcode ender,
			 get_option expression whencode,multi)
	| Ast.Edots(dots,whencode) ->
	    Ast.Edots(string_mcode dots,get_option expression whencode)
	| Ast.Ecircles(dots,whencode) ->
	    Ast.Ecircles(string_mcode dots,get_option expression whencode)
	| Ast.Estars(dots,whencode) ->
	    Ast.Estars(string_mcode dots,get_option expression whencode)
	| Ast.OptExp(exp) -> Ast.OptExp(expression exp)
	| Ast.UniqueExp(exp) -> Ast.UniqueExp(expression exp)) in
    exprfn all_functions k e

  and string_fragment e =
    let k e =
      Ast.rewrap e
	(match Ast.unwrap e with
	  Ast.ConstantFragment(str) -> Ast.ConstantFragment(string_mcode str)
	| Ast.FormatFragment(pct,fmt) ->
	    Ast.FormatFragment(string_mcode pct, string_format fmt)
	| Ast.Strdots dots -> Ast.Strdots (string_mcode dots)
	| Ast.MetaFormatList(pct,name,lenname,keep,inherited) ->
	    Ast.MetaFormatList(string_mcode pct,meta_mcode name,lenname,
			       keep,inherited)) in
    fragfn all_functions k e

  and string_format e =
    let k e =
      Ast.rewrap e
	(match Ast.unwrap e with
	  Ast.ConstantFormat(str) -> Ast.ConstantFormat(string_mcode str)
	| Ast.MetaFormat(name,constraints,keep,inherited) ->
	    Ast.MetaFormat(meta_mcode name,constraints,keep,inherited)) in
    fmtfn all_functions k e

  and fullType ft =
    let k ft =
      Ast.rewrap ft
	(match Ast.unwrap ft with
	  Ast.Type(allminus,cv,ty) ->
	    Ast.Type (allminus,get_option cv_mcode cv, typeC ty)
	| Ast.AsType(ty,asty) -> Ast.AsType(fullType ty,fullType asty)
	| Ast.DisjType(types) -> Ast.DisjType(List.map fullType types)
	| Ast.OptType(ty) -> Ast.OptType(fullType ty)
	| Ast.UniqueType(ty) -> Ast.UniqueType(fullType ty)) in
    ftfn all_functions k ft

  and typeC ty =
    let k ty =
      Ast.rewrap ty
	(match Ast.unwrap ty with
	  Ast.BaseType(ty,strings) ->
	    Ast.BaseType (ty, List.map string_mcode strings)
	| Ast.SignedT(sgn,ty) ->
	    Ast.SignedT(sign_mcode sgn,get_option typeC ty)
	| Ast.Pointer(ty,star) ->
	    Ast.Pointer (fullType ty, string_mcode star)
	| Ast.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
	    Ast.FunctionPointer(fullType ty,string_mcode lp1,string_mcode star,
				string_mcode rp1,string_mcode lp2,
				parameter_dots params,
				string_mcode rp2)
	| Ast.FunctionType(allminus,ty,lp,params,rp) ->
	    Ast.FunctionType(allminus,get_option fullType ty,string_mcode lp,
			     parameter_dots params,string_mcode rp)
	| Ast.Array(ty,lb,size,rb) ->
	    Ast.Array(fullType ty, string_mcode lb,
		      get_option expression size, string_mcode rb)
      | Ast.Decimal(dec,lp,length,comma,precision_opt,rp) ->
	  Ast.Decimal(string_mcode dec, string_mcode lp,
		      expression length, get_option string_mcode comma,
		      get_option expression precision_opt, string_mcode rp)
	| Ast.EnumName(kind,name) ->
	    Ast.EnumName(string_mcode kind, get_option ident name)
	| Ast.EnumDef(ty,lb,ids,rb) ->
	    Ast.EnumDef (fullType ty, string_mcode lb, expression_dots ids,
			 string_mcode rb)
	| Ast.StructUnionName(kind,name) ->
	    Ast.StructUnionName (struct_mcode kind, get_option ident name)
	| Ast.StructUnionDef(ty,lb,decls,rb) ->
	    Ast.StructUnionDef (fullType ty,
				string_mcode lb, declaration_dots decls,
				string_mcode rb)
	| Ast.TypeName(name) -> Ast.TypeName(string_mcode name)
	| Ast.MetaType(name,keep,inherited) ->
	    Ast.MetaType(meta_mcode name,keep,inherited)) in
    tyfn all_functions k ty

  and declaration d =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.MetaDecl(name,keep,inherited) ->
	    Ast.MetaDecl(meta_mcode name,keep,inherited)
	| Ast.MetaField(name,keep,inherited) ->
	    Ast.MetaField(meta_mcode name,keep,inherited)
	| Ast.MetaFieldList(name,lenname_inh,keep,inherited) ->
	    Ast.MetaFieldList(meta_mcode name,lenname_inh,keep,inherited)
	| Ast.AsDecl(decl,asdecl) ->
	    Ast.AsDecl(declaration decl,declaration asdecl)
	| Ast.Init(stg,ty,id,eq,ini,sem) ->
	    Ast.Init(get_option storage_mcode stg, fullType ty, ident id,
		     string_mcode eq, initialiser ini, string_mcode sem)
	| Ast.UnInit(stg,ty,id,sem) ->
	    Ast.UnInit(get_option storage_mcode stg, fullType ty, ident id,
		       string_mcode sem)
	| Ast.MacroDecl(name,lp,args,rp,sem) ->
	    Ast.MacroDecl(ident name, string_mcode lp, expression_dots args,
			  string_mcode rp,string_mcode sem)
	| Ast.MacroDeclInit(name,lp,args,rp,eq,ini,sem) ->
	    Ast.MacroDeclInit
	      (ident name, string_mcode lp, expression_dots args,
	       string_mcode rp,string_mcode eq,initialiser ini,
	       string_mcode sem)
	| Ast.TyDecl(ty,sem) -> Ast.TyDecl(fullType ty, string_mcode sem)
	| Ast.Typedef(stg,ty,id,sem) ->
	    Ast.Typedef(string_mcode stg, fullType ty, typeC id,
			string_mcode sem)
	| Ast.DisjDecl(decls) -> Ast.DisjDecl(List.map declaration decls)
	| Ast.Ddots(dots,whencode) ->
	    Ast.Ddots(string_mcode dots, get_option declaration whencode)
	| Ast.OptDecl(decl) -> Ast.OptDecl(declaration decl)
	| Ast.UniqueDecl(decl) -> Ast.UniqueDecl(declaration decl)) in
    declfn all_functions k d

  and initialiser i =
    let k i =
      Ast.rewrap i
	(match Ast.unwrap i with
	  Ast.MetaInit(name,keep,inherited) ->
	    Ast.MetaInit(meta_mcode name,keep,inherited)
	| Ast.MetaInitList(name,lenname_inh,keep,inherited) ->
	    Ast.MetaInitList(meta_mcode name,lenname_inh,keep,inherited)
	| Ast.AsInit(ini,asini) ->
	    Ast.AsInit(initialiser ini,initialiser asini)
	| Ast.InitExpr(exp) -> Ast.InitExpr(expression exp)
	| Ast.ArInitList(lb,initlist,rb) ->
	    Ast.ArInitList(string_mcode lb, initialiser_dots initlist,
			   string_mcode rb)
	| Ast.StrInitList(allminus,lb,initlist,rb,whencode) ->
	    Ast.StrInitList(allminus,
			 string_mcode lb, List.map initialiser initlist,
			 string_mcode rb, List.map initialiser whencode)
	| Ast.InitGccName(name,eq,ini) ->
	    Ast.InitGccName(ident name, string_mcode eq, initialiser ini)
	| Ast.InitGccExt(designators,eq,ini) ->
	    Ast.InitGccExt
	      (List.map designator designators, string_mcode eq,
	       initialiser ini)
	| Ast.IComma(cm) -> Ast.IComma(string_mcode cm)
	| Ast.Idots(dots,whencode) ->
	    Ast.Idots(string_mcode dots,get_option initialiser whencode)
	| Ast.OptIni(i) -> Ast.OptIni(initialiser i)
	| Ast.UniqueIni(i) -> Ast.UniqueIni(initialiser i)) in
    initfn all_functions k i

  and designator = function
      Ast.DesignatorField(dot,id) ->
	Ast.DesignatorField(string_mcode dot,ident id)
    | Ast.DesignatorIndex(lb,exp,rb) ->
	Ast.DesignatorIndex(string_mcode lb,expression exp,string_mcode rb)
    | Ast.DesignatorRange(lb,min,dots,max,rb) ->
	Ast.DesignatorRange(string_mcode lb,expression min,string_mcode dots,
			     expression max,string_mcode rb)

  and parameterTypeDef p =
    let k p =
      Ast.rewrap p
	(match Ast.unwrap p with
	  Ast.VoidParam(ty) -> Ast.VoidParam(fullType ty)
	| Ast.Param(ty,id) -> Ast.Param(fullType ty, get_option ident id)
	| Ast.MetaParam(name,keep,inherited) ->
	    Ast.MetaParam(meta_mcode name,keep,inherited)
	| Ast.MetaParamList(name,lenname_inh,keep,inherited) ->
	    Ast.MetaParamList(meta_mcode name,lenname_inh,keep,inherited)
	| Ast.AsParam(p,asexp) ->
	    Ast.AsParam(parameterTypeDef p, expression asexp)
	| Ast.PComma(cm) -> Ast.PComma(string_mcode cm)
	| Ast.Pdots(dots) -> Ast.Pdots(string_mcode dots)
	| Ast.Pcircles(dots) -> Ast.Pcircles(string_mcode dots)
	| Ast.OptParam(param) -> Ast.OptParam(parameterTypeDef param)
	| Ast.UniqueParam(param) -> Ast.UniqueParam(parameterTypeDef param)) in
    paramfn all_functions k p

  and rule_elem re =
    let k re =
      Ast.rewrap re
	(match Ast.unwrap re with
	  Ast.FunHeader(bef,allminus,fi,name,lp,params,rp) ->
	    Ast.FunHeader(bef,allminus,List.map fninfo fi,ident name,
			  string_mcode lp, parameter_dots params,
			  string_mcode rp)
	| Ast.Decl(bef,allminus,decl) ->
	    Ast.Decl(bef,allminus,declaration decl)
	| Ast.SeqStart(brace) -> Ast.SeqStart(string_mcode brace)
	| Ast.SeqEnd(brace) -> Ast.SeqEnd(string_mcode brace)
	| Ast.ExprStatement(exp,sem) ->
	    Ast.ExprStatement (get_option expression exp, string_mcode sem)
	| Ast.IfHeader(iff,lp,exp,rp) ->
	    Ast.IfHeader(string_mcode iff, string_mcode lp, expression exp,
	      string_mcode rp)
	| Ast.Else(els) -> Ast.Else(string_mcode els)
	| Ast.WhileHeader(whl,lp,exp,rp) ->
	    Ast.WhileHeader(string_mcode whl, string_mcode lp, expression exp,
			    string_mcode rp)
	| Ast.DoHeader(d) -> Ast.DoHeader(string_mcode d)
	| Ast.WhileTail(whl,lp,exp,rp,sem) ->
	    Ast.WhileTail(string_mcode whl, string_mcode lp, expression exp,
			  string_mcode rp, string_mcode sem)
	| Ast.ForHeader(fr,lp,first,e2,sem2,e3,rp) ->
	    let first = forinfo first in
	    Ast.ForHeader(string_mcode fr, string_mcode lp, first,
			  get_option expression e2, string_mcode sem2,
			  get_option expression e3, string_mcode rp)
	| Ast.IteratorHeader(whl,lp,args,rp) ->
	    Ast.IteratorHeader(ident whl, string_mcode lp,
			       expression_dots args, string_mcode rp)
	| Ast.SwitchHeader(switch,lp,exp,rp) ->
	    Ast.SwitchHeader(string_mcode switch, string_mcode lp,
			     expression exp, string_mcode rp)
	| Ast.Break(br,sem) ->
	    Ast.Break(string_mcode br, string_mcode sem)
	| Ast.Continue(cont,sem) ->
	    Ast.Continue(string_mcode cont, string_mcode sem)
	| Ast.Label(l,dd) -> Ast.Label(ident l, string_mcode dd)
	| Ast.Goto(goto,l,sem) ->
	    Ast.Goto(string_mcode goto,ident l,string_mcode sem)
	| Ast.Return(ret,sem) ->
	    Ast.Return(string_mcode ret, string_mcode sem)
	| Ast.ReturnExpr(ret,exp,sem) ->
	    Ast.ReturnExpr(string_mcode ret, expression exp, string_mcode sem)
	| Ast.Exec(exec,lang,code,sem) ->
	    Ast.Exec(string_mcode exec,string_mcode lang,
		     exec_code_dots code,string_mcode sem)
	| Ast.MetaStmt(name,keep,seqible,inherited) ->
	    Ast.MetaStmt(meta_mcode name,keep,seqible,inherited)
	| Ast.MetaStmtList(name,keep,inherited) ->
	    Ast.MetaStmtList(meta_mcode name,keep,inherited)
	| Ast.MetaRuleElem(name,keep,inherited) ->
	    Ast.MetaRuleElem(meta_mcode name,keep,inherited)
	| Ast.Exp(exp) -> Ast.Exp(expression exp)
	| Ast.TopExp(exp) -> Ast.TopExp(expression exp)
	| Ast.Ty(ty) -> Ast.Ty(fullType ty)
	| Ast.TopInit(init) -> Ast.TopInit(initialiser init)
	| Ast.Include(inc,name) ->
	    Ast.Include(string_mcode inc,inc_file_mcode name)
	| Ast.Undef(def,id) ->
	    Ast.Undef(string_mcode def,ident id)
	| Ast.DefineHeader(def,id,params) ->
	    Ast.DefineHeader(string_mcode def,ident id,
			     define_parameters params)
	| Ast.Pragma(prg,id,body) ->
	    Ast.Pragma(string_mcode prg,ident id,pragmainfo body)
	| Ast.Default(def,colon) ->
	    Ast.Default(string_mcode def,string_mcode colon)
	| Ast.Case(case,exp,colon) ->
	    Ast.Case(string_mcode case,expression exp,string_mcode colon)
	| Ast.DisjRuleElem(res) -> Ast.DisjRuleElem(List.map rule_elem res)) in
    rulefn all_functions k re

  (* not parameterizable for now... *)
  and forinfo fi =
    let k = function
      Ast.ForExp(e1,sem1) ->
	Ast.ForExp(get_option expression e1,string_mcode sem1)
    | Ast.ForDecl (bef,allminus,decl) ->
	Ast.ForDecl(bef,allminus,declaration decl) in
    k fi

  (* not parameterizable for now... *)
  and pragmainfo pi =
    let k pi =
      Ast.rewrap pi
	(match Ast.unwrap pi with
	  Ast.PragmaTuple(lp,args,rp) ->
	    Ast.PragmaTuple(string_mcode lp,expression_dots args,
			    string_mcode rp)
	| Ast.PragmaIdList(ids) -> Ast.PragmaIdList(identifier_dots ids)
	| Ast.PragmaDots (dots) -> Ast.PragmaDots(string_mcode dots)) in
    k pi

  (* not parameterizable for now... *)
  and define_parameters p =
    let k p =
      Ast.rewrap p
	(match Ast.unwrap p with
	  Ast.NoParams -> Ast.NoParams
	| Ast.DParams(lp,params,rp) ->
	    Ast.DParams(string_mcode lp,define_param_dots params,
			string_mcode rp)) in
    k p

  and define_param_dots d =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.DOTS(l) -> Ast.DOTS(List.map define_param l)
	| Ast.CIRCLES(l) -> Ast.CIRCLES(List.map define_param l)
	| Ast.STARS(l) -> Ast.STARS(List.map define_param l)) in
    k d

  and define_param p =
    let k p =
      Ast.rewrap p
	(match Ast.unwrap p with
	  Ast.DParam(id) -> Ast.DParam(ident id)
	| Ast.DPComma(comma) -> Ast.DPComma(string_mcode comma)
	| Ast.DPdots(d) -> Ast.DPdots(string_mcode d)
	| Ast.DPcircles(c) -> Ast.DPcircles(string_mcode c)
	| Ast.OptDParam(dp) -> Ast.OptDParam(define_param dp)
	| Ast.UniqueDParam(dp) -> Ast.UniqueDParam(define_param dp)) in
    k p

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
	    Ast.Seq(rule_elem lbrace,
		    statement_dots body, rule_elem rbrace)
	| Ast.IfThen(header,branch,aft) ->
	    Ast.IfThen(rule_elem header, statement branch,aft)
	| Ast.IfThenElse(header,branch1,els,branch2,aft) ->
	    Ast.IfThenElse(rule_elem header, statement branch1, rule_elem els,
			   statement branch2, aft)
	| Ast.While(header,body,aft) ->
	    Ast.While(rule_elem header, statement body, aft)
	| Ast.Do(header,body,tail) ->
	    Ast.Do(rule_elem header, statement body, rule_elem tail)
	| Ast.For(header,body,aft) ->
	    Ast.For(rule_elem header, statement body, aft)
	| Ast.Iterator(header,body,aft) ->
	    Ast.Iterator(rule_elem header, statement body, aft)
	| Ast.Switch(header,lb,decls,cases,rb) ->
	    Ast.Switch(rule_elem header,rule_elem lb,
		       statement_dots decls,
		       List.map case_line cases,rule_elem rb)
	| Ast.Atomic(re) -> Ast.Atomic(rule_elem re)
	| Ast.Disj(stmt_dots_list) ->
	    Ast.Disj (List.map statement_dots stmt_dots_list)
	| Ast.Nest(starter,stmt_dots,ender,whn,multi,bef,aft) ->
	    Ast.Nest(string_mcode starter,statement_dots stmt_dots,
		     string_mcode ender,
		     List.map (whencode statement_dots statement) whn,
		     multi,bef,aft)
	| Ast.FunDecl(header,lbrace,body,rbrace) ->
	    Ast.FunDecl(rule_elem header,rule_elem lbrace,
			statement_dots body, rule_elem rbrace)
	| Ast.Define(header,body) ->
	    Ast.Define(rule_elem header,statement_dots body)
	| Ast.AsStmt(stm,asstm) -> Ast.AsStmt(statement stm,statement asstm)
	| Ast.Dots(d,whn,bef,aft) ->
	    Ast.Dots(string_mcode d,
		     List.map (whencode statement_dots statement) whn,bef,aft)
	| Ast.Circles(d,whn,bef,aft) ->
	    Ast.Circles(string_mcode d,
			List.map (whencode statement_dots statement) whn,
			bef,aft)
	| Ast.Stars(d,whn,bef,aft) ->
	    Ast.Stars(string_mcode d,
		      List.map (whencode statement_dots statement) whn,bef,aft)
	| Ast.OptStm(stmt) -> Ast.OptStm(statement stmt)
	| Ast.UniqueStm(stmt) -> Ast.UniqueStm(statement stmt)) in
    let s = stmtfn all_functions k s in
    (* better to do this after, in case there is an equality test on the whole
       statement, eg in free_vars.  equality test would require that this
       subterm not already be changed *)
    process_bef_aft s

  and fninfo = function
      Ast.FStorage(stg) -> Ast.FStorage(storage_mcode stg)
    | Ast.FType(ty) -> Ast.FType(fullType ty)
    | Ast.FInline(inline) -> Ast.FInline(string_mcode inline)
    | Ast.FAttr(attr) -> Ast.FAttr(string_mcode attr)

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
	    Ast.CaseLine(rule_elem header,statement_dots code)
	| Ast.OptCase(case) -> Ast.OptCase(case_line case)) in
    casefn all_functions k c

  and exec_code e =
    (* not configurable *)
    Ast.rewrap e
      (match Ast.unwrap e with
	Ast.ExecEval(colon,id) ->
	  Ast.ExecEval(string_mcode colon, expression id)
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
      | Ast.FixOpTag(fixop) as x -> x
      | Ast.BinaryOpTag(binop) as x -> x
      | Ast.ArithOpTag(arithop) as x -> x
      | Ast.LogicalOpTag(logop) as x -> x
      | Ast.InitTag(decl) -> Ast.InitTag(initialiser decl)
      | Ast.DeclarationTag(decl) -> Ast.DeclarationTag(declaration decl)
      | Ast.StorageTag(stg) as x -> x
      | Ast.IncFileTag(stg) as x -> x
      | Ast.Rule_elemTag(rule) -> Ast.Rule_elemTag(rule_elem rule)
      | Ast.StatementTag(rule) -> Ast.StatementTag(statement rule)
      | Ast.ForInfoTag(rule) -> Ast.ForInfoTag(forinfo rule)
      | Ast.CaseLineTag(case) -> Ast.CaseLineTag(case_line case)
      | Ast.StringFragmentTag(frag) ->
	  Ast.StringFragmentTag(string_fragment frag)
      | Ast.ConstVolTag(cv) as x -> x
      | Ast.Token(tok,info) as x -> x
      | Ast.Directive(str) as x -> x
      | Ast.Code(cd) -> Ast.Code(top_level cd)
      | Ast.ExprDotsTag(ed) -> Ast.ExprDotsTag(expression_dots ed)
      | Ast.ParamDotsTag(pd) -> Ast.ParamDotsTag(parameter_dots pd)
      | Ast.StmtDotsTag(sd) -> Ast.StmtDotsTag(statement_dots sd)
      | Ast.DeclDotsTag(sd) -> Ast.DeclDotsTag(declaration_dots sd)
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
      rebuilder_fullType = fullType;
      rebuilder_typeC = typeC;
      rebuilder_declaration = declaration;
      rebuilder_initialiser = initialiser;
      rebuilder_parameter = parameterTypeDef;
      rebuilder_parameter_list = parameter_dots;
      rebuilder_rule_elem = rule_elem;
      rebuilder_statement = statement;
      rebuilder_case_line = case_line;
      rebuilder_top_level = top_level;
      rebuilder_expression_dots = expression_dots;
      rebuilder_statement_dots = statement_dots;
      rebuilder_declaration_dots = declaration_dots;
      rebuilder_initialiser_dots = initialiser_dots;
      rebuilder_define_param_dots = define_param_dots;
      rebuilder_define_param = define_param;
      rebuilder_define_parameters = define_parameters;
      rebuilder_anything = anything} in
  all_functions

