module Ast = Ast_cocci
module Ast0 = Ast0_cocci

(* --------------------------------------------------------------------- *)
(* Generic traversal: combiner *)
(* parameters:
   combining function
   treatment of: mcode, identifiers, expressions, typeCs, types,
   declarations, statements, toplevels
   default value for options *)

type 'a combiner =
    {combiner_ident : Ast0.ident -> 'a;
      combiner_expression : Ast0.expression -> 'a;
	combiner_typeC : Ast0.typeC -> 'a;
	  combiner_declaration : Ast0.declaration -> 'a;
	    combiner_parameter : Ast0.parameterTypeDef -> 'a;
	      combiner_parameter_list : Ast0.parameter_list -> 'a;
		combiner_statement : Ast0.statement -> 'a;
		  combiner_top_level : Ast0.top_level -> 'a;
		    combiner_expression_dots :
		      Ast0.expression Ast0.dots -> 'a;
			combiner_statement_dots :
			  Ast0.statement Ast0.dots -> 'a}


type ('mc,'a) cmcode = 'mc Ast0_cocci.mcode -> 'a
type ('cd,'a) ccode = 'a combiner -> ('cd -> 'a) -> 'cd -> 'a

let combiner bind option_default 
    string_mcode const_mcode assign_mcode fix_mcode unary_mcode binary_mcode
    cv_mcode base_mcode sign_mcode struct_mcode storage_mcode
    dotsexprfn dotsparamfn dotsstmtfn
    identfn exprfn tyfn paramfn declfn stmtfn topfn =
  let multibind l = List.fold_right bind l option_default in
  let get_option f = function
      Some x -> f x
    | None -> option_default in
  let rec expression_dots d =
    let k d =
      match Ast0.unwrap d with
	Ast0.DOTS(l) -> multibind (List.map expression l)
      | Ast0.CIRCLES(l) -> multibind (List.map expression l)
      | Ast0.STARS(l) -> multibind (List.map expression l) in
    dotsexprfn all_functions k d
  and parameter_dots d =
    let k d =
      match Ast0.unwrap d with
	Ast0.DOTS(l) -> multibind (List.map parameterTypeDef l)
      | Ast0.CIRCLES(l) -> multibind (List.map parameterTypeDef l)
      | Ast0.STARS(l) -> multibind (List.map parameterTypeDef l) in
    dotsparamfn all_functions k d
  and statement_dots d =
    let k d =
      match Ast0.unwrap d with
	Ast0.DOTS(l) -> multibind (List.map statement l)
      | Ast0.CIRCLES(l) -> multibind (List.map statement l)
      | Ast0.STARS(l) -> multibind (List.map statement l) in
    dotsstmtfn all_functions k d
  and ident i =
    let k i =
      match Ast0.unwrap i with
	Ast0.Id(name) -> string_mcode name
      | Ast0.MetaId(name) -> string_mcode name
      | Ast0.MetaFunc(name) -> string_mcode name
      | Ast0.MetaLocalFunc(name) -> string_mcode name
      | Ast0.OptIdent(id) -> ident id
      | Ast0.UniqueIdent(id) -> ident id
      | Ast0.MultiIdent(id) -> ident id in
  identfn all_functions k i
  and expression e =
    let k e =
      match Ast0.unwrap e with
	Ast0.Ident(id) -> ident id
      | Ast0.Constant(const) -> const_mcode const
      | Ast0.FunCall(fn,lp,args,rp) ->
	  multibind
	    [expression fn; string_mcode lp; expression_dots args;
	      string_mcode rp]
      | Ast0.Assignment(left,op,right) ->
	  multibind [expression left; assign_mcode op; expression right]
      | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
	  multibind
	    [expression exp1; string_mcode why; get_option expression exp2;
	      string_mcode colon; expression exp3]
      | Ast0.Postfix(exp,op) -> bind (expression exp) (fix_mcode op)
      | Ast0.Infix(exp,op) -> bind (fix_mcode op) (expression exp)
      | Ast0.Unary(exp,op) -> bind (unary_mcode op) (expression exp)
      | Ast0.Binary(left,op,right) ->
	  multibind [expression left; binary_mcode op; expression right]
      | Ast0.Paren(lp,exp,rp) ->
	  multibind [string_mcode lp; expression exp; string_mcode rp]
      | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
	  multibind
	    [expression exp1; string_mcode lb; expression exp2;
	      string_mcode rb]
      | Ast0.RecordAccess(exp,pt,field) ->
	  multibind [expression exp; string_mcode pt; ident field]
      | Ast0.RecordPtAccess(exp,ar,field) ->
	  multibind [expression exp; string_mcode ar; ident field]
      | Ast0.Cast(lp,ty,rp,exp) ->
	  multibind
	    [string_mcode lp; typeC ty; string_mcode rp; expression exp]
      | Ast0.SizeOfExpr(szf,exp) ->
	  multibind [string_mcode szf; expression exp]
      | Ast0.SizeOfType(szf,lp,ty,rp) ->
	  multibind
	    [string_mcode szf; string_mcode lp; typeC ty; string_mcode rp]
      | Ast0.MetaConst(name,ty) -> string_mcode name
      | Ast0.MetaErr(name) -> string_mcode name
      | Ast0.MetaExpr(name,ty) -> string_mcode name
      | Ast0.MetaExprList(name) -> string_mcode name
      | Ast0.EComma(cm) -> string_mcode cm
      | Ast0.DisjExpr(starter,expr_list,ender) ->
	  bind (string_mcode starter)
	    (bind (multibind (List.map expression expr_list))
	        (string_mcode ender))
      | Ast0.NestExpr(starter,expr_dots,ender) ->
	  bind (string_mcode starter)
	    (bind (expression_dots expr_dots) (string_mcode ender))
      | Ast0.Edots(dots,whencode) | Ast0.Ecircles(dots,whencode)
      | Ast0.Estars(dots,whencode) ->
	  bind (string_mcode dots) (get_option expression whencode)
      | Ast0.OptExp(exp) -> expression exp
      | Ast0.UniqueExp(exp) -> expression exp
      | Ast0.MultiExp(exp) -> expression exp in
    exprfn all_functions k e
  and typeC t =
    let k t =
      match Ast0.unwrap t with
	Ast0.ConstVol(cv,ty) -> bind (cv_mcode cv) (typeC ty)
      |	Ast0.BaseType(ty,sign) ->
	  bind (get_option sign_mcode sign) (base_mcode ty)
      | Ast0.Pointer(ty,star) -> bind (typeC ty) (string_mcode star)
      | Ast0.Array(ty,lb,size,rb) ->
	  multibind
	    [typeC ty; string_mcode lb; get_option expression size;
	      string_mcode rb]
      | Ast0.StructUnionName(name,kind) ->
	  bind (struct_mcode kind) (string_mcode name)
      | Ast0.TypeName(name) -> string_mcode name
      | Ast0.MetaType(name) -> string_mcode name
      | Ast0.OptType(ty) -> typeC ty
      | Ast0.UniqueType(ty) -> typeC ty
      | Ast0.MultiType(ty) -> typeC ty in
    tyfn all_functions k t
  and declaration d =
    let k d =
      match Ast0.unwrap d with
	Ast0.Init(ty,id,eq,exp,sem) ->
	  multibind [typeC ty; ident id; string_mcode eq; expression exp;
		      string_mcode sem]
      | Ast0.UnInit(ty,id,sem) ->
	  multibind [typeC ty; ident id; string_mcode sem]
      |	Ast0.DisjDecl(starter,decls,ender) ->
	  bind (string_mcode starter)
	    (bind (multibind (List.map declaration decls))
	       (string_mcode ender))
      | Ast0.OptDecl(decl) -> declaration decl
      | Ast0.UniqueDecl(decl) -> declaration decl
      | Ast0.MultiDecl(decl) -> declaration decl in
    declfn all_functions k d
  and parameterTypeDef p =
    let k p =
      match Ast0.unwrap p with
	Ast0.VoidParam(ty) -> typeC ty
      | Ast0.Param(id,ty) -> bind (typeC ty) (ident id)
      | Ast0.MetaParam(name) -> string_mcode name
      | Ast0.MetaParamList(name) -> string_mcode name
      | Ast0.PComma(cm) -> string_mcode cm
      | Ast0.Pdots(dots) -> string_mcode dots
      | Ast0.Pcircles(dots) -> string_mcode dots
      | Ast0.OptParam(param) -> parameterTypeDef param
      | Ast0.UniqueParam(param) -> parameterTypeDef param in
    paramfn all_functions k p
  and statement s =
    let k s =
      match Ast0.unwrap s with
	Ast0.FunDecl(stg,ty,name,lp,params,rp,lbrace,body,rbrace) ->
	  multibind
	    [get_option storage_mcode stg; get_option typeC ty;
	      ident name; string_mcode lp;
	      parameter_dots params; string_mcode rp; string_mcode lbrace;
	      statement_dots body; string_mcode rbrace]
      | Ast0.Decl(decl) -> declaration decl
      | Ast0.Seq(lbrace,body,rbrace) ->
	  multibind
	    [string_mcode lbrace; statement_dots body; string_mcode rbrace]
      | Ast0.ExprStatement(exp,sem) ->
	  bind (expression exp) (string_mcode sem)
      | Ast0.IfThen(iff,lp,exp,rp,branch1,_) ->
	  multibind
	    [string_mcode iff; string_mcode lp; expression exp;
	      string_mcode rp; statement branch1]
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,_) ->
	  multibind
	    [string_mcode iff; string_mcode lp; expression exp;
	      string_mcode rp; statement branch1; string_mcode els;
	      statement branch2]
      | Ast0.While(whl,lp,exp,rp,body,_) ->
	  multibind
	    [string_mcode whl; string_mcode lp; expression exp;
	      string_mcode rp; statement body]
      | Ast0.Do(d,body,whl,lp,exp,rp,sem) ->
	  multibind
	    [string_mcode d; statement body; string_mcode whl;
	      string_mcode lp; expression exp; string_mcode rp;
	      string_mcode sem]
      | Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body,_) ->
	  multibind
	    [string_mcode fr; string_mcode lp; get_option expression e1;
	      string_mcode sem1; get_option expression e2; string_mcode sem2;
	      get_option expression e3;
	      string_mcode rp; statement body]
      | Ast0.Return(ret,sem) -> bind (string_mcode ret) (string_mcode sem)
      | Ast0.ReturnExpr(ret,exp,sem) ->
	  multibind [string_mcode ret; expression exp; string_mcode sem]
      | Ast0.MetaStmt(name) -> string_mcode name
      | Ast0.MetaStmtList(name) -> string_mcode name
      | Ast0.Disj(starter,statement_dots_list,ender) ->
	  bind (string_mcode starter)
	    (bind (multibind (List.map statement_dots statement_dots_list))
	       (string_mcode ender))
      | Ast0.Nest(starter,stmt_dots,ender) ->
	  bind (string_mcode starter)
	    (bind (statement_dots stmt_dots) (string_mcode ender))
      | Ast0.Exp(exp) -> expression exp
      | Ast0.Dots(d,whencode) | Ast0.Circles(d,whencode)
      | Ast0.Stars(d,whencode) ->
	  bind (string_mcode d) (get_option statement_dots whencode)
      | Ast0.OptStm(re) -> statement re
      | Ast0.UniqueStm(re) -> statement re
      | Ast0.MultiStm(re) -> statement re in
    stmtfn all_functions k s
  and top_level t =
    let k t =
      match Ast0.unwrap t with
	Ast0.DECL(decl) -> declaration decl
      | Ast0.INCLUDE(inc,name) -> bind (string_mcode inc) (string_mcode name)
      | Ast0.FILEINFO(old_file,new_file) ->
	  bind (string_mcode old_file) (string_mcode new_file)
      | Ast0.FUNCTION(stmt_dots) -> statement stmt_dots
      | Ast0.CODE(stmt_dots) -> statement_dots stmt_dots
      | Ast0.ERRORWORDS(exps) -> multibind (List.map expression exps)
      | Ast0.OTHER(_) -> failwith "unexpected code" in
    topfn all_functions k t
  and all_functions =
    {combiner_ident = ident;
      combiner_expression = expression;
      combiner_typeC = typeC;
      combiner_declaration = declaration;
      combiner_parameter = parameterTypeDef;
      combiner_parameter_list = parameter_dots;
      combiner_statement = statement;
      combiner_top_level = top_level;
      combiner_expression_dots = expression_dots;
      combiner_statement_dots = statement_dots} in
  all_functions

(* --------------------------------------------------------------------- *)
(* Generic traversal: rebuilder *)

type 'a inout = 'a -> 'a (* for specifying the type of rebuilder *)

type rebuilder =
    {rebuilder_ident : Ast0_cocci.ident inout;
      rebuilder_expression : Ast0_cocci.expression inout;
      rebuilder_typeC : Ast0_cocci.typeC inout;
      rebuilder_declaration : Ast0_cocci.declaration inout;
      rebuilder_parameter : Ast0_cocci.parameterTypeDef inout;
      rebuilder_parameter_list : Ast0_cocci.parameter_list inout;
      rebuilder_statement : Ast0_cocci.statement inout;
      rebuilder_top_level : Ast0_cocci.top_level inout;
      rebuilder_expression_dots :
	Ast0_cocci.expression Ast0_cocci.dots ->
	  Ast0_cocci.expression Ast0_cocci.dots;
	  rebuilder_statement_dots :
	    Ast0_cocci.statement Ast0_cocci.dots ->
	      Ast0_cocci.statement Ast0_cocci.dots}

type 'mc rmcode = 'mc Ast0_cocci.mcode inout
type 'cd rcode = rebuilder -> ('cd inout) -> 'cd inout

let rebuilder = fun
    string_mcode const_mcode assign_mcode fix_mcode unary_mcode binary_mcode
    cv_mcode base_mcode sign_mcode struct_mcode storage_mcode
    dotsexprfn dotsparamfn dotsstmtfn
    identfn exprfn tyfn paramfn declfn stmtfn topfn ->
  let get_option f = function
      Some x -> Some (f x)
    | None -> None in
  let rec expression_dots d =
    let k d =
      Ast0.rewrap d
	(match Ast0.unwrap d with
	  Ast0.DOTS(l) -> Ast0.DOTS(List.map expression l)
	| Ast0.CIRCLES(l) -> Ast0.CIRCLES(List.map expression l)
	| Ast0.STARS(l) -> Ast0.STARS(List.map expression l)) in
    dotsexprfn all_functions k d
  and parameter_list d =
    let k d =
      Ast0.rewrap d
	(match Ast0.unwrap d with
	  Ast0.DOTS(l) -> Ast0.DOTS(List.map parameterTypeDef l)
	| Ast0.CIRCLES(l) -> Ast0.CIRCLES(List.map parameterTypeDef l)
	| Ast0.STARS(l) -> Ast0.STARS(List.map parameterTypeDef l)) in
    dotsparamfn all_functions k d
  and statement_dots d =
    let k d =
      Ast0.rewrap d
	(match Ast0.unwrap d with
	  Ast0.DOTS(l) -> Ast0.DOTS(List.map statement l)
	| Ast0.CIRCLES(l) -> Ast0.CIRCLES(List.map statement l)
	| Ast0.STARS(l) -> Ast0.STARS(List.map statement l)) in
    dotsstmtfn all_functions k d
  and ident i =
    let k i =
      Ast0.rewrap i
	(match Ast0.unwrap i with
	  Ast0.Id(name) -> Ast0.Id(string_mcode name)
	| Ast0.MetaId(name) -> Ast0.MetaId(string_mcode name)
	| Ast0.MetaFunc(name) -> Ast0.MetaFunc(string_mcode name)
	| Ast0.MetaLocalFunc(name) -> Ast0.MetaLocalFunc(string_mcode name)
	| Ast0.OptIdent(id) -> Ast0.OptIdent(ident id)
	| Ast0.UniqueIdent(id) -> Ast0.UniqueIdent(ident id)
	| Ast0.MultiIdent(id) -> Ast0.MultiIdent(ident id)) in
    identfn all_functions k i
  and expression e =
    let k e =
      Ast0.rewrap e
	(match Ast0.unwrap e with
	  Ast0.Ident(id) -> Ast0.Ident(ident id)
	| Ast0.Constant(const) -> Ast0.Constant(const_mcode const)
	| Ast0.FunCall(fn,lp,args,rp) ->
	    Ast0.FunCall(expression fn,string_mcode lp,expression_dots args,
			 string_mcode rp)
	| Ast0.Assignment(left,op,right) ->
	    Ast0.Assignment(expression left,assign_mcode op,expression right)
	| Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
	    Ast0.CondExpr(expression exp1, string_mcode why,
			  get_option expression exp2, string_mcode colon,
			  expression exp3)
	| Ast0.Postfix(exp,op) -> Ast0.Postfix(expression exp, fix_mcode op)
	| Ast0.Infix(exp,op) -> Ast0.Infix(expression exp, fix_mcode op)
	| Ast0.Unary(exp,op) -> Ast0.Unary(expression exp, unary_mcode op)
	| Ast0.Binary(left,op,right) ->
	    Ast0.Binary(expression left, binary_mcode op, expression right)
	| Ast0.Paren(lp,exp,rp) ->
	    Ast0.Paren(string_mcode lp, expression exp, string_mcode rp)
	| Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
	    Ast0.ArrayAccess(expression exp1,string_mcode lb,expression exp2,
			     string_mcode rb)
	| Ast0.RecordAccess(exp,pt,field) ->
	    Ast0.RecordAccess(expression exp, string_mcode pt, ident field)
	| Ast0.RecordPtAccess(exp,ar,field) ->
	    Ast0.RecordPtAccess(expression exp, string_mcode ar, ident field)
	| Ast0.Cast(lp,ty,rp,exp) ->
	    Ast0.Cast(string_mcode lp, typeC ty, string_mcode rp,
		      expression exp)
	| Ast0.SizeOfExpr(szf,exp) ->
	    Ast0.SizeOfExpr(string_mcode szf, expression exp)
	| Ast0.SizeOfType(szf,lp,ty,rp) ->
	    Ast0.SizeOfType(string_mcode szf,string_mcode lp, typeC ty, 
                           string_mcode rp)
	| Ast0.MetaConst(name,ty) ->
	    Ast0.MetaConst(string_mcode name,ty)
	| Ast0.MetaErr(name) -> Ast0.MetaErr(string_mcode name)
	| Ast0.MetaExpr(name,ty) -> Ast0.MetaExpr(string_mcode name,ty)
	| Ast0.MetaExprList(name) -> Ast0.MetaExprList(string_mcode name)
	| Ast0.EComma(cm) -> Ast0.EComma(string_mcode cm)
	| Ast0.DisjExpr(starter,expr_list,ender) ->
	    Ast0.DisjExpr(string_mcode starter,List.map expression expr_list,
			  string_mcode ender)
	| Ast0.NestExpr(starter,expr_dots,ender) ->
	    Ast0.NestExpr(string_mcode starter,expression_dots expr_dots,
			  string_mcode ender)
	| Ast0.Edots(dots,whencode) ->
	    Ast0.Edots(string_mcode dots, get_option expression whencode)
	| Ast0.Ecircles(dots,whencode) ->
	    Ast0.Ecircles(string_mcode dots, get_option expression whencode)
	| Ast0.Estars(dots,whencode) ->
	    Ast0.Estars(string_mcode dots, get_option expression whencode)
	| Ast0.OptExp(exp) -> Ast0.OptExp(expression exp)
	| Ast0.UniqueExp(exp) -> Ast0.UniqueExp(expression exp)
	| Ast0.MultiExp(exp) -> Ast0.MultiExp(expression exp)) in
    exprfn all_functions k e
  and typeC t =
    let k t =
      Ast0.rewrap t
	(match Ast0.unwrap t with
	  Ast0.ConstVol(cv,ty) -> Ast0.ConstVol(cv_mcode cv,typeC ty)
	| Ast0.BaseType(ty,sign) ->
	    Ast0.BaseType(base_mcode ty, get_option sign_mcode sign)
	| Ast0.Pointer(ty,star) ->
	    Ast0.Pointer(typeC ty, string_mcode star)
	| Ast0.Array(ty,lb,size,rb) ->
	    Ast0.Array(typeC ty, string_mcode lb,
		       get_option expression size, string_mcode rb)
	| Ast0.StructUnionName(name,kind) ->
	    Ast0.StructUnionName(string_mcode name, struct_mcode kind)
	| Ast0.TypeName(name) -> Ast0.TypeName(string_mcode name)
	| Ast0.MetaType(name) -> Ast0.MetaType(string_mcode name)
	| Ast0.OptType(ty) -> Ast0.OptType(typeC ty)
	| Ast0.UniqueType(ty) -> Ast0.UniqueType(typeC ty)
	| Ast0.MultiType(ty) -> Ast0.MultiType(typeC ty)) in
    tyfn all_functions k t
  and declaration d =
    let k d =
      Ast0.rewrap d
	(match Ast0.unwrap d with
	  Ast0.Init(ty,id,eq,exp,sem) ->
	    Ast0.Init(typeC ty, ident id, string_mcode eq, expression exp,
		      string_mcode sem)
	| Ast0.UnInit(ty,id,sem) ->
	    Ast0.UnInit(typeC ty, ident id, string_mcode sem)
	| Ast0.DisjDecl(starter,decls,ender) ->
	    Ast0.DisjDecl(string_mcode starter,List.map declaration decls,
			  string_mcode ender)
	| Ast0.OptDecl(decl) -> Ast0.OptDecl(declaration decl)
	| Ast0.UniqueDecl(decl) -> Ast0.UniqueDecl(declaration decl)
	| Ast0.MultiDecl(decl) -> Ast0.MultiDecl(declaration decl)) in
    declfn all_functions k d
  and parameterTypeDef p =
    let k p =
      Ast0.rewrap p
	(match Ast0.unwrap p with
	  Ast0.VoidParam(ty) -> Ast0.VoidParam(typeC ty)
	| Ast0.Param(id,ty) -> Ast0.Param(ident id, typeC ty)
	| Ast0.MetaParam(name) -> Ast0.MetaParam(string_mcode name)
	| Ast0.MetaParamList(name) -> Ast0.MetaParamList(string_mcode name)
	| Ast0.PComma(cm) -> Ast0.PComma(string_mcode cm)
	| Ast0.Pdots(dots) -> Ast0.Pdots(string_mcode dots)
	| Ast0.Pcircles(dots) -> Ast0.Pcircles(string_mcode dots)
	| Ast0.OptParam(param) -> Ast0.OptParam(parameterTypeDef param)
	| Ast0.UniqueParam(param) ->
	    Ast0.UniqueParam(parameterTypeDef param)) in
    paramfn all_functions k p
  and statement s =
    let k s =
      Ast0.rewrap s
	(match Ast0.unwrap s with
	  Ast0.FunDecl(stg,ty,name,lp,params,rp,lbrace,body,rbrace) ->
	    Ast0.FunDecl(get_option storage_mcode stg,
			 get_option typeC ty, ident name,
			 string_mcode lp, parameter_list params,
			 string_mcode rp, string_mcode lbrace,
			 statement_dots body, string_mcode rbrace)
	| Ast0.Decl(decl) -> Ast0.Decl(declaration decl)
	| Ast0.Seq(lbrace,body,rbrace) ->
	    Ast0.Seq(string_mcode lbrace, statement_dots body,
		     string_mcode rbrace)
	| Ast0.ExprStatement(exp,sem) ->
	    Ast0.ExprStatement(expression exp, string_mcode sem)
	| Ast0.IfThen(iff,lp,exp,rp,branch1,aft) ->
	    Ast0.IfThen(string_mcode iff, string_mcode lp, expression exp,
	      string_mcode rp, statement branch1,aft)
	| Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) ->
	    Ast0.IfThenElse(string_mcode iff,string_mcode lp,expression exp,
	      string_mcode rp, statement branch1, string_mcode els,
	      statement branch2,aft)
	| Ast0.While(whl,lp,exp,rp,body,aft) ->
	    Ast0.While(string_mcode whl, string_mcode lp, expression exp,
		       string_mcode rp, statement body, aft)
	| Ast0.Do(d,body,whl,lp,exp,rp,sem) ->
	    Ast0.Do(string_mcode d, statement body, string_mcode whl,
		    string_mcode lp, expression exp, string_mcode rp,
		    string_mcode sem)
	| Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body,aft) ->
	    Ast0.For(string_mcode fr, string_mcode lp,
		     get_option expression e1, string_mcode sem1,
		     get_option expression e2, string_mcode sem2,
		     get_option expression e3,
		     string_mcode rp, statement body, aft)
	| Ast0.Return(ret,sem) ->
	    Ast0.Return(string_mcode ret,string_mcode sem)
	| Ast0.ReturnExpr(ret,exp,sem) ->
	    Ast0.ReturnExpr(string_mcode ret,expression exp,string_mcode sem)
	| Ast0.MetaStmt(name) -> Ast0.MetaStmt(string_mcode name)
	| Ast0.MetaStmtList(name) -> Ast0.MetaStmtList(string_mcode name)
	| Ast0.Disj(starter,statement_dots_list,ender) ->
	    Ast0.Disj(string_mcode starter,
		      List.map statement_dots statement_dots_list,
		      string_mcode ender)
	| Ast0.Nest(starter,stmt_dots,ender) ->
	    Ast0.Nest(string_mcode starter,statement_dots stmt_dots,
		      string_mcode ender)
	| Ast0.Exp(exp) -> Ast0.Exp(expression exp)
	| Ast0.Dots(d,whencode) ->
	    Ast0.Dots(string_mcode d, get_option statement_dots whencode)
	| Ast0.Circles(d,whencode) ->
	    Ast0.Circles(string_mcode d, get_option statement_dots whencode)
	| Ast0.Stars(d,whencode) ->
	    Ast0.Stars(string_mcode d, get_option statement_dots whencode)
	| Ast0.OptStm(re) -> Ast0.OptStm(statement re)
	| Ast0.UniqueStm(re) -> Ast0.UniqueStm(statement re)
	| Ast0.MultiStm(re) -> Ast0.MultiStm(statement re)) in
    stmtfn all_functions k s
  and top_level t =
    let k t =
      Ast0.rewrap t
	(match Ast0.unwrap t with
	  Ast0.DECL(decl) -> Ast0.DECL(declaration decl)
	| Ast0.INCLUDE(inc,name) ->
	    Ast0.INCLUDE(string_mcode inc, string_mcode name)
	| Ast0.FILEINFO(old_file,new_file) ->
	    Ast0.FILEINFO(string_mcode old_file, string_mcode new_file)
	| Ast0.FUNCTION(statement_dots) ->
	    Ast0.FUNCTION(statement statement_dots)
	| Ast0.CODE(stmt_dots) -> Ast0.CODE(statement_dots stmt_dots)
	| Ast0.ERRORWORDS(exps) -> Ast0.ERRORWORDS(List.map expression exps)
	| Ast0.OTHER(_) -> failwith "unexpected code") in
    topfn all_functions k t
  and all_functions =
    {rebuilder_ident = ident;
      rebuilder_expression = expression;
      rebuilder_typeC = typeC;
      rebuilder_declaration = declaration;
      rebuilder_parameter = parameterTypeDef;
      rebuilder_parameter_list = parameter_list;
      rebuilder_statement = statement;
      rebuilder_top_level = top_level;
      rebuilder_expression_dots = expression_dots;
      rebuilder_statement_dots = statement_dots} in
  all_functions
