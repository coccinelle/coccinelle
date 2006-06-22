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

type ('mc,'a) cmcode = 'mc Ast_cocci.mcode -> 'a
type ('cd,'a) ccode = 'a combiner -> ('cd -> 'a) -> 'cd -> 'a


let combiner bind option_default 
    string_mcode const_mcode assign_mcode fix_mcode unary_mcode binary_mcode
    cv_mcode base_mcode sign_mcode struct_mcode storage_mcode
    expdotsfn paramdotsfn stmtdotsfn
    identfn exprfn ftfn tyfn paramfn declfn rulefn stmtfn topfn anyfn =
  let multibind l = List.fold_right bind l option_default in
  let get_option f = function
      Some x -> f x
    | None -> option_default in
  let rec expression_dots d =
    let k = function
      Ast.DOTS(l) | Ast.CIRCLES(l) | Ast.STARS(l) ->
	multibind (List.map expression l) in
    expdotsfn all_functions k d

  and parameter_dots d =
    let k = function
      Ast.DOTS(l) | Ast.CIRCLES(l) | Ast.STARS(l) ->
	multibind (List.map parameterTypeDef l) in
    paramdotsfn all_functions k d

  and statement_dots d =
    let k = function
      Ast.DOTS(l) | Ast.CIRCLES(l) | Ast.STARS(l) ->
	multibind (List.map statement l) in
    stmtdotsfn all_functions k d

  and ident i =
    let k = function
	Ast.Id(name) -> string_mcode name
      | Ast.MetaId(name) -> string_mcode name
      | Ast.MetaFunc(name) -> string_mcode name
      | Ast.MetaLocalFunc(name) -> string_mcode name
      | Ast.OptIdent(id) -> ident id
      | Ast.UniqueIdent(id) -> ident id
      | Ast.MultiIdent(id) -> ident id in
    identfn all_functions k i
    
  and expression e =
    let k = function
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
      | Ast.MetaConst(name,_) -> string_mcode name
      | Ast.MetaErr(name) -> string_mcode name
      | Ast.MetaExpr(name,_) -> string_mcode name
      | Ast.MetaExprList(name) -> string_mcode name
      | Ast.EComma(cm) -> string_mcode cm
      | Ast.DisjExpr(exp_list) -> multibind (List.map expression exp_list)
      | Ast.NestExpr(expr_dots) -> expression_dots expr_dots
      | Ast.Edots(dots,whencode) | Ast.Ecircles(dots,whencode)
      | Ast.Estars(dots,whencode) ->
	  bind (string_mcode dots) (get_option expression whencode)
      | Ast.OptExp(exp) | Ast.UniqueExp(exp) | Ast.MultiExp(exp) ->
	  expression exp in
    exprfn all_functions k e
	  
  and fullType ft =
    let k = function
	Ast.Type(cv,ty) -> bind (get_option cv_mcode cv) (typeC ty)
      | Ast.OptType(ty) -> fullType ty
      | Ast.UniqueType(ty) -> fullType ty
      | Ast.MultiType(ty) -> fullType ty in
    ftfn all_functions k ft
	  
  and typeC ty =
    let k = function
	Ast.BaseType(ty,sgn) ->
	  bind (get_option sign_mcode sgn) (base_mcode ty)
      | Ast.Pointer(ty,star) -> bind (fullType ty) (string_mcode star)
      | Ast.Array(ty,lb,size,rb) ->
	  multibind [fullType ty; string_mcode lb;
		      get_option expression size; string_mcode rb]
      | Ast.StructUnionName(name,kind) ->
	  bind (struct_mcode kind) (string_mcode name)
      | Ast.TypeName(name) -> string_mcode name
      | Ast.MetaType(name) -> string_mcode name in
    tyfn all_functions k ty
	  
  and declaration d =
    let k = function
	Ast.Init(ty,id,eq,exp,sem) ->
	  multibind [fullType ty; ident id; string_mcode eq; expression exp;
		      string_mcode sem]
      | Ast.UnInit(ty,id,sem) ->
	  multibind [fullType ty; ident id; string_mcode sem]
      | Ast.OptDecl(decl) -> declaration decl
      | Ast.UniqueDecl(decl) -> declaration decl
      | Ast.MultiDecl(decl) -> declaration decl in
    declfn all_functions k d
	  
  and parameterTypeDef p =
    let k = function
	Ast.VoidParam(ty) -> fullType ty
      | Ast.Param(id,ty) -> bind (fullType ty) (ident id)
      | Ast.MetaParam(name) -> string_mcode name
      | Ast.MetaParamList(name) -> string_mcode name
      | Ast.PComma(cm) -> string_mcode cm
      | Ast.Pdots(dots) -> string_mcode dots
      | Ast.Pcircles(dots) -> string_mcode dots
      | Ast.OptParam(param) -> parameterTypeDef param
      | Ast.UniqueParam(param) -> parameterTypeDef param in
    paramfn all_functions k p

  and rule_elem re =
    let k = function
	Ast.FunHeader(stg,name,lp,params,rp) ->
	  multibind [get_option storage_mcode stg; ident name;
		      string_mcode lp; parameter_dots params;
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
      | Ast.MetaStmt(name) -> string_mcode name
      | Ast.MetaStmtList(name) -> string_mcode name
      | Ast.Exp(exp) -> expression exp in
    rulefn all_functions k re

  and statement s =
    let k = function
	Ast.Seq(lbrace,body,rbrace) ->
	  multibind [rule_elem lbrace; statement_dots body; rule_elem rbrace]
      | Ast.IfThen(header,branch) ->
	  multibind [rule_elem header; statement branch]
      | Ast.IfThenElse(header,branch1,els,branch2) ->
	  multibind [rule_elem header; statement branch1; rule_elem els;
		      statement branch2]
      | Ast.While(header,body) -> multibind [rule_elem header; statement body]
      | Ast.Do(header,body,tail) ->
	  multibind [rule_elem header; statement body; rule_elem tail]
      | Ast.For(header,body) -> multibind [rule_elem header; statement body]
      | Ast.Atomic(re) -> rule_elem re
      | Ast.Disj(stmt_dots_list) ->
	  multibind (List.map statement_dots stmt_dots_list)
      | Ast.Nest(stmt_dots) -> statement_dots stmt_dots
      | Ast.FunDecl(header,lbrace,body,rbrace) ->
	  multibind [rule_elem header; rule_elem lbrace; statement_dots body;
		      rule_elem rbrace]
      | Ast.Dots(d,whencode,_) | Ast.Circles(d,whencode,_)
      | Ast.Stars(d,whencode,_) ->
	  multibind [string_mcode d; get_option statement_dots whencode]
      | Ast.OptStm(stmt) | Ast.UniqueStm(stmt) | Ast.MultiStm(stmt) ->
	  statement stmt in
    stmtfn all_functions k s
    
  and top_level t =
    let k = function
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
      | Ast.Code(cd) -> top_level cd in
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
