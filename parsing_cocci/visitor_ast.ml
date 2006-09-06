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
	combiner_fullType : Ast.fullType -> 'a;
	  combiner_typeC : Ast.typeC -> 'a;
	    combiner_declaration : Ast.declaration -> 'a;
	      combiner_parameter_list : Ast.parameter_list -> 'a;
		combiner_rule_elem : Ast.rule_elem -> 'a;
		  combiner_statement : Ast.statement -> 'a;
		    combiner_top_level : Ast.top_level -> 'a;
		      combiner_anything : Ast.anything  -> 'a;
			combiner_expression_dots :
			  Ast.expression Ast.dots -> 'a;
			    combiner_statement_dots :
			      Ast.statement Ast.dots -> 'a}

type ('mc,'a) cmcode = 'a combiner -> 'mc Ast_cocci.mcode -> 'a
type ('cd,'a) ccode = 'a combiner -> ('cd -> 'a) -> 'cd -> 'a


let combiner bind option_default 
    string_mcodefn const_mcodefn assign_mcodefn fix_mcodefn unary_mcodefn
    binary_mcodefn
    cv_mcodefn base_mcodefn sign_mcodefn struct_mcodefn storage_mcodefn
    expdotsfn paramdotsfn stmtdotsfn
    identfn exprfn ftfn tyfn paramfn declfn rulefn stmtfn topfn anyfn =
  let multibind l = List.fold_right bind l option_default in
  let get_option f = function
      Some x -> f x
    | None -> option_default in

  let rec string_mcode x = string_mcodefn all_functions x
  and const_mcode x = const_mcodefn all_functions x
  and assign_mcode x = assign_mcodefn all_functions x
  and fix_mcode x = fix_mcodefn all_functions x
  and unary_mcode x = unary_mcodefn all_functions x
  and binary_mcode x = binary_mcodefn all_functions x
  and cv_mcode x = cv_mcodefn all_functions x
  and base_mcode x = base_mcodefn all_functions x
  and sign_mcode x = sign_mcodefn all_functions x
  and struct_mcode x = struct_mcodefn all_functions x
  and storage_mcode x = storage_mcodefn all_functions x

  and expression_dots d =
    let k d =
      match Ast.unwrap d with
	Ast.DOTS(l) | Ast.CIRCLES(l) | Ast.STARS(l) ->
	  multibind (List.map expression l) in
    expdotsfn all_functions k d

  and parameter_dots d =
    let k d =
      match Ast.unwrap d with
	Ast.DOTS(l) | Ast.CIRCLES(l) | Ast.STARS(l) ->
	  multibind (List.map parameterTypeDef l) in
    paramdotsfn all_functions k d

  and statement_dots d =
    let k d =
      match Ast.unwrap d with
	Ast.DOTS(l) | Ast.CIRCLES(l) | Ast.STARS(l) ->
	  multibind (List.map statement l) in
    stmtdotsfn all_functions k d

  and ident i =
    let k i =
      match Ast.unwrap i with
	Ast.Id(name) -> string_mcode name
      | Ast.MetaId(name,_) -> string_mcode name
      | Ast.MetaFunc(name,_) -> string_mcode name
      | Ast.MetaLocalFunc(name,_) -> string_mcode name
      | Ast.OptIdent(id) -> ident id
      | Ast.UniqueIdent(id) -> ident id
      | Ast.MultiIdent(id) -> ident id in
    identfn all_functions k i
    
  and expression e =
    let k e =
      match Ast.unwrap e with
	Ast.Ident(id) -> ident id
      | Ast.Constant(const) -> const_mcode const
      | Ast.FunCall(fn,lp,args,rp) ->
	  multibind [expression fn; string_mcode lp; expression_dots args;
		      string_mcode rp]
      | Ast.Assignment(left,op,right) ->
	  multibind [expression left; assign_mcode op; expression right]
      | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
	  multibind [expression exp1; string_mcode why;
		      get_option expression exp2; string_mcode colon;
		      expression exp3]
      | Ast.Postfix(exp,op) -> bind (expression exp) (fix_mcode op)
      | Ast.Infix(exp,op) -> bind (fix_mcode op) (expression exp)
      | Ast.Unary(exp,op) -> bind (unary_mcode op) (expression exp)
      | Ast.Binary(left,op,right) ->
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
      | Ast.MetaConst(name,_,_) -> string_mcode name
      | Ast.MetaErr(name,_) -> string_mcode name
      | Ast.MetaExpr(name,_,_) -> string_mcode name
      | Ast.MetaExprList(name,_) -> string_mcode name
      | Ast.EComma(cm) -> string_mcode cm
      | Ast.DisjExpr(exp_list) -> multibind (List.map expression exp_list)
      | Ast.NestExpr(expr_dots,whencode) ->
	  bind (expression_dots expr_dots) (get_option expression whencode)
      | Ast.Edots(dots,whencode) | Ast.Ecircles(dots,whencode)
      | Ast.Estars(dots,whencode) ->
	  bind (string_mcode dots) (get_option expression whencode)
      | Ast.OptExp(exp) | Ast.UniqueExp(exp) | Ast.MultiExp(exp) ->
	  expression exp in
    exprfn all_functions k e
	  
  and fullType ft =
    let k ft =
      match Ast.unwrap ft with
	Ast.Type(cv,ty) -> bind (get_option cv_mcode cv) (typeC ty)
      | Ast.OptType(ty) -> fullType ty
      | Ast.UniqueType(ty) -> fullType ty
      | Ast.MultiType(ty) -> fullType ty in
    ftfn all_functions k ft
	  
  and typeC ty =
    let k ty =
      match Ast.unwrap ty with
	Ast.BaseType(ty,sgn) ->
	  bind (get_option sign_mcode sgn) (base_mcode ty)
      | Ast.Pointer(ty,star) ->
	  bind (fullType ty) (string_mcode star)
      | Ast.Array(ty,lb,size,rb) ->
	  multibind [fullType ty; string_mcode lb;
		      get_option expression size; string_mcode rb]
      | Ast.StructUnionName(name,kind) ->
	  bind (struct_mcode kind) (string_mcode name)
      | Ast.TypeName(name) -> string_mcode name
      | Ast.MetaType(name,_) -> string_mcode name in
    tyfn all_functions k ty
	  
  and declaration d =
    let k d =
      match Ast.unwrap d with
	Ast.Init(ty,id,eq,exp,sem) ->
	  multibind [fullType ty; ident id; string_mcode eq; expression exp;
		      string_mcode sem]
      | Ast.UnInit(ty,id,sem) ->
	  multibind [fullType ty; ident id; string_mcode sem]
      | Ast.DisjDecl(decls) -> multibind (List.map declaration decls)
      | Ast.MetaDecl(name,_) -> string_mcode name
      | Ast.OptDecl(decl) -> declaration decl
      | Ast.UniqueDecl(decl) -> declaration decl
      | Ast.MultiDecl(decl) -> declaration decl in
    declfn all_functions k d
	  
  and parameterTypeDef p =
    let k p =
      match Ast.unwrap p with
	Ast.VoidParam(ty) -> fullType ty
      | Ast.Param(id,ty) -> bind (fullType ty) (ident id)
      | Ast.MetaParam(name,_) -> string_mcode name
      | Ast.MetaParamList(name,_) -> string_mcode name
      | Ast.PComma(cm) -> string_mcode cm
      | Ast.Pdots(dots) -> string_mcode dots
      | Ast.Pcircles(dots) -> string_mcode dots
      | Ast.OptParam(param) -> parameterTypeDef param
      | Ast.UniqueParam(param) -> parameterTypeDef param in
    paramfn all_functions k p

  and rule_elem re =
    let k re =
      match Ast.unwrap re with
	Ast.FunHeader(_,stg,ty,name,lp,params,rp) ->
	  multibind [get_option storage_mcode stg; get_option fullType ty;
		      ident name; string_mcode lp; parameter_dots params;
		      string_mcode rp]
      | Ast.Decl(decl) -> declaration decl
      | Ast.SeqStart(brace) -> string_mcode brace
      | Ast.SeqEnd(brace) -> string_mcode brace
      | Ast.ExprStatement(exp,sem) ->
	  bind (expression exp) (string_mcode sem)
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
      | Ast.ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp) ->
	  multibind [string_mcode fr; string_mcode lp; 
		      get_option expression e1; string_mcode sem1; 
		      get_option expression e2; string_mcode sem2; 
		      get_option expression e3; string_mcode rp]
      | Ast.Return(ret,sem) ->
	  bind (string_mcode ret) (string_mcode sem)
      | Ast.ReturnExpr(ret,exp,sem) ->
	  multibind [string_mcode ret; expression exp; string_mcode sem]
      | Ast.MetaStmt(name,_,_) -> string_mcode name
      | Ast.MetaStmtList(name,_) -> string_mcode name
      | Ast.MetaRuleElem(name,_) -> string_mcode name
      | Ast.Exp(exp) -> expression exp in
    rulefn all_functions k re

  and statement s =
    let k s =
      match Ast.unwrap s with
	Ast.Seq(lbrace,decls,dots,body,rbrace) ->
	  multibind [rule_elem lbrace; statement_dots decls;
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
      | Ast.Atomic(re) -> rule_elem re
      | Ast.Disj(stmt_dots_list) ->
	  multibind (List.map statement_dots stmt_dots_list)
      | Ast.Nest(stmt_dots,whencode,_) ->
	  multibind ((statement_dots stmt_dots) ::
		     (List.map statement_dots whencode))
      | Ast.FunDecl(header,lbrace,decls,dots,body,rbrace) ->
	  multibind [rule_elem header; rule_elem lbrace;
		      statement_dots decls; statement_dots body;
		      rule_elem rbrace]
      | Ast.Dots(d,whencode,_) | Ast.Circles(d,whencode,_)
      | Ast.Stars(d,whencode,_) ->
	  multibind ((string_mcode d) :: (List.map statement_dots whencode))
      | Ast.OptStm(stmt) | Ast.UniqueStm(stmt) | Ast.MultiStm(stmt) ->
	  statement stmt in
    stmtfn all_functions k s
    
  and top_level t =
    let k t =
      match Ast.unwrap t with
	Ast.DECL(decl) -> declaration decl
      | Ast.INCLUDE(inc,name) -> bind (string_mcode inc) (string_mcode name)
      | Ast.FILEINFO(old_file,new_file) ->
	  bind (string_mcode old_file) (string_mcode new_file)
      | Ast.FUNCTION(stmt) -> statement stmt
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
      | Ast.ParameterTypeDefTag(ptd) -> parameterTypeDef ptd
      | Ast.StorageTag(stg) -> option_default
      | Ast.Rule_elemTag(rule) -> rule_elem rule
      | Ast.StatementTag(rule) -> statement rule
      | Ast.ConstVolTag(cv) -> option_default
      | Ast.Token(tok) -> option_default
      | Ast.Code(cd) -> top_level cd
      | Ast.ExprDotsTag(ed) -> expression_dots ed
      | Ast.ParamDotsTag(pd) -> parameter_dots pd
      | Ast.StmtDotsTag(sd) -> statement_dots sd
      | Ast.TypeCTag(ty) -> typeC ty
      | Ast.ParamTag(param) -> parameterTypeDef param in
    anyfn all_functions k a

  and all_functions =
    {combiner_ident = ident;
      combiner_expression = expression;
      combiner_fullType = fullType;
      combiner_typeC = typeC;
      combiner_declaration = declaration;
      combiner_parameter_list = parameter_dots;
      combiner_rule_elem = rule_elem;
      combiner_statement = statement;
      combiner_top_level = top_level;
      combiner_anything = anything;
      combiner_expression_dots = expression_dots;
      combiner_statement_dots = statement_dots} in
  all_functions

(* ---------------------------------------------------------------------- *)

type 'a inout = 'a -> 'a (* for specifying the type of rebuilder *)

type rebuilder =
    {rebuilder_ident : Ast.ident inout;
      rebuilder_expression : Ast.expression inout;
      rebuilder_typeC : Ast.typeC inout;
      rebuilder_declaration : Ast.declaration inout;
      rebuilder_parameter : Ast.parameterTypeDef inout;
      rebuilder_parameter_list : Ast.parameter_list inout;
      rebuilder_statement : Ast.statement inout;
      rebuilder_rule_elem : Ast.rule_elem inout;
      rebuilder_top_level : Ast.top_level inout;
      rebuilder_expression_dots : Ast.expression Ast.dots inout;
      rebuilder_statement_dots : Ast.statement Ast.dots inout;
      rebuilder_anything : Ast.anything inout}

type 'mc rmcode = 'mc Ast.mcode inout
type 'cd rcode = rebuilder -> ('cd inout) -> 'cd inout


let rebuilder
    string_mcode const_mcode assign_mcode fix_mcode unary_mcode binary_mcode
    cv_mcode base_mcode sign_mcode struct_mcode storage_mcode
    expdotsfn paramdotsfn stmtdotsfn
    identfn exprfn ftfn tyfn paramfn declfn rulefn stmtfn topfn anyfn =
  let get_option f = function
      Some x -> Some (f x)
    | None -> None in
  let rec expression_dots d =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.DOTS(l) -> Ast.DOTS(List.map expression l)
	| Ast.CIRCLES(l) -> Ast.CIRCLES(List.map expression l)
	| Ast.STARS(l) -> Ast.STARS(List.map expression l)) in
    expdotsfn all_functions k d

  and parameter_dots d =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.DOTS(l) -> Ast.DOTS(List.map parameterTypeDef l)
	| Ast.CIRCLES(l) -> Ast.CIRCLES(List.map parameterTypeDef l)
	| Ast.STARS(l) -> Ast.STARS(List.map parameterTypeDef l)) in
    paramdotsfn all_functions k d

  and statement_dots d =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.DOTS(l) -> Ast.DOTS(List.map statement l)
	| Ast.CIRCLES(l) -> Ast.CIRCLES(List.map statement l)
	| Ast.STARS(l) -> Ast.STARS(List.map statement l)) in
    stmtdotsfn all_functions k d

  and ident i =
    let k i =
      Ast.rewrap i
	(match Ast.unwrap i with
	  Ast.Id(name) -> Ast.Id(string_mcode name)
	| Ast.MetaId(name,inherited) ->
	    Ast.MetaId(string_mcode name,inherited)
	| Ast.MetaFunc(name,inherited) ->
	    Ast.MetaFunc(string_mcode name,inherited)
	| Ast.MetaLocalFunc(name,inherited) ->
	    Ast.MetaLocalFunc(string_mcode name,inherited)
	| Ast.OptIdent(id) -> Ast.OptIdent(ident id)
	| Ast.UniqueIdent(id) -> Ast.UniqueIdent(ident id)
	| Ast.MultiIdent(id) -> Ast.MultiIdent(ident id)) in
    identfn all_functions k i
      
  and expression e =
    let k e =
      Ast.rewrap e
	(match Ast.unwrap e with
	  Ast.Ident(id) -> Ast.Ident(ident id)
	| Ast.Constant(const) -> Ast.Constant(const_mcode const)
	| Ast.FunCall(fn,lp,args,rp) ->
	    Ast.FunCall(expression fn, string_mcode lp, expression_dots args,
			string_mcode rp)
	| Ast.Assignment(left,op,right) ->
	    Ast.Assignment(expression left, assign_mcode op, expression right)
	| Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
	    Ast.CondExpr(expression exp1, string_mcode why,
			 get_option expression exp2, string_mcode colon,
			 expression exp3)
	| Ast.Postfix(exp,op) -> Ast.Postfix(expression exp,fix_mcode op)
	| Ast.Infix(exp,op) -> Ast.Infix(expression exp,fix_mcode op)
	| Ast.Unary(exp,op) -> Ast.Unary(expression exp,unary_mcode op)
	| Ast.Binary(left,op,right) ->
	    Ast.Binary(expression left, binary_mcode op, expression right)
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
	| Ast.MetaConst(name,ty,inherited) ->
	    Ast.MetaConst(string_mcode name,ty,inherited)
	| Ast.MetaErr(name,inherited) ->
	    Ast.MetaErr(string_mcode name,inherited)
	| Ast.MetaExpr(name,ty,inherited) ->
	    Ast.MetaExpr(string_mcode name,ty,inherited)
	| Ast.MetaExprList(name,inherited) ->
	    Ast.MetaExprList(string_mcode name,inherited)
	| Ast.EComma(cm) -> Ast.EComma(string_mcode cm)
	| Ast.DisjExpr(exp_list) -> Ast.DisjExpr(List.map expression exp_list)
	| Ast.NestExpr(expr_dots,whencode) ->
	    Ast.NestExpr(expression_dots expr_dots,
			 get_option expression whencode)
	| Ast.Edots(dots,whencode) ->
	    Ast.Edots(string_mcode dots,get_option expression whencode)
	| Ast.Ecircles(dots,whencode) ->
	    Ast.Ecircles(string_mcode dots,get_option expression whencode)
	| Ast.Estars(dots,whencode) ->
	    Ast.Estars(string_mcode dots,get_option expression whencode)
	| Ast.OptExp(exp) -> Ast.OptExp(expression exp)
	| Ast.UniqueExp(exp) -> Ast.UniqueExp(expression exp)
	| Ast.MultiExp(exp) -> Ast.MultiExp(expression exp)) in
    exprfn all_functions k e
	  
  and fullType ft =
    let k ft =
      Ast.rewrap ft
	(match Ast.unwrap ft with
	  Ast.Type(cv,ty) -> Ast.Type (get_option cv_mcode cv, typeC ty)
	| Ast.OptType(ty) -> Ast.OptType(fullType ty)
	| Ast.UniqueType(ty) -> Ast.UniqueType(fullType ty)
	| Ast.MultiType(ty) -> Ast.MultiType(fullType ty)) in
    ftfn all_functions k ft
	  
  and typeC ty =
    let k ty =
      Ast.rewrap ty
	(match Ast.unwrap ty with
	  Ast.BaseType(ty,sgn) ->
	    Ast.BaseType (base_mcode ty,get_option sign_mcode sgn)
	| Ast.Pointer(ty,star) ->
	    Ast.Pointer (fullType ty, string_mcode star)
	| Ast.Array(ty,lb,size,rb) ->
	    Ast.Array(fullType ty, string_mcode lb,
		      get_option expression size, string_mcode rb)
	| Ast.StructUnionName(name,kind) ->
	    Ast.StructUnionName (string_mcode name, struct_mcode kind)
	| Ast.TypeName(name) -> Ast.TypeName(string_mcode name)
	| Ast.MetaType(name,inherited) ->
	    Ast.MetaType(string_mcode name,inherited)) in
    tyfn all_functions k ty
	  
  and declaration d =
    let k d =
      Ast.rewrap d
	(match Ast.unwrap d with
	  Ast.Init(ty,id,eq,exp,sem) ->
	    Ast.Init(fullType ty, ident id, string_mcode eq, expression exp,
		     string_mcode sem)
	| Ast.UnInit(ty,id,sem) ->
	    Ast.UnInit(fullType ty, ident id, string_mcode sem)
	| Ast.DisjDecl(decls) -> Ast.DisjDecl(List.map declaration decls)
	| Ast.MetaDecl(name,inherited) ->
	    Ast.MetaDecl(string_mcode name,inherited)
	| Ast.OptDecl(decl) -> Ast.OptDecl(declaration decl)
	| Ast.UniqueDecl(decl) -> Ast.UniqueDecl(declaration decl)
	| Ast.MultiDecl(decl) -> Ast.MultiDecl(declaration decl)) in
    declfn all_functions k d
	  
  and parameterTypeDef p =
    let k p =
      Ast.rewrap p
	(match Ast.unwrap p with
	  Ast.VoidParam(ty) -> Ast.VoidParam(fullType ty)
	| Ast.Param(id,ty) -> Ast.Param(ident id, fullType ty)
	| Ast.MetaParam(name,inherited) ->
	    Ast.MetaParam(string_mcode name,inherited)
	| Ast.MetaParamList(name,inherited) ->
	    Ast.MetaParamList(string_mcode name,inherited)
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
	  Ast.FunHeader(allminus,stg,ty,name,lp,params,rp) ->
	    Ast.FunHeader(allminus,get_option storage_mcode stg,
			  get_option fullType ty, ident name,
			  string_mcode lp, parameter_dots params,
			  string_mcode rp)
	| Ast.Decl(decl) -> Ast.Decl(declaration decl)
	| Ast.SeqStart(brace) -> Ast.SeqStart(string_mcode brace)
	| Ast.SeqEnd(brace) -> Ast.SeqEnd(string_mcode brace)
	| Ast.ExprStatement(exp,sem) ->
	    Ast.ExprStatement (expression exp, string_mcode sem)
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
	| Ast.ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp) ->
	    Ast.ForHeader(string_mcode fr, string_mcode lp, 
			  get_option expression e1, string_mcode sem1, 
			  get_option expression e2, string_mcode sem2, 
			  get_option expression e3, string_mcode rp)
	| Ast.Return(ret,sem) ->
	    Ast.Return(string_mcode ret, string_mcode sem)
	| Ast.ReturnExpr(ret,exp,sem) ->
	    Ast.ReturnExpr(string_mcode ret, expression exp, string_mcode sem)
	| Ast.MetaStmt(name,seqible,inherited) ->
	    Ast.MetaStmt(string_mcode name,seqible,inherited)
	| Ast.MetaStmtList(name,inherited) ->
	    Ast.MetaStmtList(string_mcode name,inherited)
	| Ast.MetaRuleElem(name,inherited) ->
	    Ast.MetaRuleElem(string_mcode name,inherited)
	| Ast.Exp(exp) -> Ast.Exp(expression exp)) in
    rulefn all_functions k re

  and statement s =
    let k s =
      Ast.rewrap s
	(match Ast.unwrap s with
	  Ast.Seq(lbrace,decls,dots,body,rbrace) ->
	    Ast.Seq(rule_elem lbrace, statement_dots decls, dots,
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
	| Ast.Atomic(re) -> Ast.Atomic(rule_elem re)
	| Ast.Disj(stmt_dots_list) ->
	    Ast.Disj (List.map statement_dots stmt_dots_list)
	| Ast.Nest(stmt_dots,whencode,t) ->
	    Ast.Nest(statement_dots stmt_dots,
		     List.map statement_dots whencode,t)
	| Ast.FunDecl(header,lbrace,decls,dots,body,rbrace) ->
	    Ast.FunDecl(rule_elem header,rule_elem lbrace,
			statement_dots decls, dots,
			statement_dots body, rule_elem rbrace)
	| Ast.Dots(d,whencode,t) ->
	    Ast.Dots(string_mcode d, List.map statement_dots whencode, t)
	| Ast.Circles(d,whencode,t) ->
	    Ast.Circles(string_mcode d, List.map statement_dots whencode, t)
	| Ast.Stars(d,whencode,t) ->
	    Ast.Stars(string_mcode d, List.map statement_dots whencode, t)
	| Ast.OptStm(stmt) -> Ast.OptStm(statement stmt)
	| Ast.UniqueStm(stmt) -> Ast.UniqueStm(statement stmt)
	| Ast.MultiStm(stmt) -> Ast.MultiStm(statement stmt)) in
    stmtfn all_functions k s
    
  and top_level t =
    let k t =
      Ast.rewrap t
	(match Ast.unwrap t with
	  Ast.DECL(decl) -> Ast.DECL(declaration decl)
	| Ast.INCLUDE(inc,name) ->
	    Ast.INCLUDE(string_mcode inc, string_mcode name)
	| Ast.FILEINFO(old_file,new_file) ->
	    Ast.FILEINFO (string_mcode old_file, string_mcode new_file)
	| Ast.FUNCTION(stmt) -> Ast.FUNCTION(statement stmt)
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
      | Ast.DeclarationTag(decl) -> Ast.DeclarationTag(declaration decl)
      | Ast.ParameterTypeDefTag(ptd) ->
	  Ast.ParameterTypeDefTag(parameterTypeDef ptd)
      | Ast.StorageTag(stg) as x -> x
      | Ast.Rule_elemTag(rule) -> Ast.Rule_elemTag(rule_elem rule)
      | Ast.StatementTag(rule) -> Ast.StatementTag(statement rule)
      | Ast.ConstVolTag(cv) as x -> x
      | Ast.Token(tok) as x -> x
      | Ast.Code(cd) -> Ast.Code(top_level cd)
      | Ast.ExprDotsTag(ed) -> Ast.ExprDotsTag(expression_dots ed)
      | Ast.ParamDotsTag(pd) -> Ast.ParamDotsTag(parameter_dots pd)
      | Ast.StmtDotsTag(sd) -> Ast.StmtDotsTag(statement_dots sd)
      | Ast.TypeCTag(ty) -> Ast.TypeCTag(typeC ty)
      | Ast.ParamTag(param) -> Ast.ParamTag(parameterTypeDef param) in
    anyfn all_functions k a

  and all_functions =
    {rebuilder_ident = ident;
      rebuilder_expression = expression;
      rebuilder_typeC = typeC;
      rebuilder_declaration = declaration;
      rebuilder_parameter = parameterTypeDef;
      rebuilder_parameter_list = parameter_dots;
      rebuilder_rule_elem = rule_elem;
      rebuilder_statement = statement;
      rebuilder_top_level = top_level;
      rebuilder_expression_dots = expression_dots;
      rebuilder_statement_dots = statement_dots;
      rebuilder_anything = anything} in
  all_functions
