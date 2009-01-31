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
      combiner_initialiser : Ast0.initialiser -> 'a;
      combiner_initialiser_list : Ast0.initialiser_list -> 'a;
      combiner_parameter : Ast0.parameterTypeDef -> 'a;
      combiner_parameter_list : Ast0.parameter_list -> 'a;
      combiner_statement : Ast0.statement -> 'a;
      combiner_case_line : Ast0.case_line -> 'a;
      combiner_top_level : Ast0.top_level -> 'a;
      combiner_expression_dots :
	  Ast0.expression Ast0.dots -> 'a;
      combiner_statement_dots :
	      Ast0.statement Ast0.dots -> 'a;
      combiner_declaration_dots :
		  Ast0.declaration Ast0.dots -> 'a;
      combiner_case_line_dots :
		  Ast0.case_line Ast0.dots -> 'a;
      combiner_anything : Ast0.anything -> 'a}


type ('mc,'a) cmcode = 'mc Ast0.mcode -> 'a
type ('cd,'a) ccode = 'a combiner -> ('cd -> 'a) -> 'cd -> 'a

let combiner bind option_default
    meta_mcode string_mcode const_mcode assign_mcode fix_mcode unary_mcode
    binary_mcode cv_mcode sign_mcode struct_mcode storage_mcode
    inc_mcode
    dotsexprfn dotsinitfn dotsparamfn dotsstmtfn dotsdeclfn dotscasefn
    identfn exprfn
    tyfn initfn paramfn declfn stmtfn casefn topfn =
  let multibind l =
    let rec loop = function
	[] -> option_default
      |	[x] -> x
      |	x::xs -> bind x (loop xs) in
    loop l in
  let get_option f = function
      Some x -> f x
    | None -> option_default in
  let rec expression_dots d =
    let k d =
      match Ast0.unwrap d with
	Ast0.DOTS(l) | Ast0.CIRCLES(l) | Ast0.STARS(l) ->
	  multibind (List.map expression l) in
    dotsexprfn all_functions k d
  and initialiser_dots d =
    let k d =
      match Ast0.unwrap d with
	Ast0.DOTS(l) | Ast0.CIRCLES(l) | Ast0.STARS(l) ->
	  multibind (List.map initialiser l) in
    dotsinitfn all_functions k d
  and parameter_dots d =
    let k d =
      match Ast0.unwrap d with
	Ast0.DOTS(l) | Ast0.CIRCLES(l) | Ast0.STARS(l) ->
	  multibind (List.map parameterTypeDef l) in
    dotsparamfn all_functions k d
  and statement_dots d =
    let k d =
      match Ast0.unwrap d with
	Ast0.DOTS(l) | Ast0.CIRCLES(l) | Ast0.STARS(l) ->
	  multibind (List.map statement l) in
    dotsstmtfn all_functions k d
  and declaration_dots d =
    let k d =
      match Ast0.unwrap d with
	Ast0.DOTS(l) | Ast0.CIRCLES(l) | Ast0.STARS(l) ->
	  multibind (List.map declaration l) in
    dotsdeclfn all_functions k d
  and case_line_dots d =
    let k d =
      match Ast0.unwrap d with
	Ast0.DOTS(l) | Ast0.CIRCLES(l) | Ast0.STARS(l) ->
	  multibind (List.map case_line l) in
    dotscasefn all_functions k d
  and ident i =
    let k i =
      match Ast0.unwrap i with
	Ast0.Id(name) -> string_mcode name
      | Ast0.MetaId(name,_,_) -> meta_mcode name
      | Ast0.MetaFunc(name,_,_) -> meta_mcode name
      | Ast0.MetaLocalFunc(name,_,_) -> meta_mcode name
      | Ast0.OptIdent(id) -> ident id
      | Ast0.UniqueIdent(id) -> ident id in
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
      | Ast0.Assignment(left,op,right,_) ->
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
      | Ast0.Nested(left,op,right) ->
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
      | Ast0.TypeExp(ty) -> typeC ty
      | Ast0.MetaErr(name,_,_)
      | Ast0.MetaExpr(name,_,_,_,_)
      | Ast0.MetaExprList(name,_,_) -> meta_mcode name
      | Ast0.EComma(cm) -> string_mcode cm
      | Ast0.DisjExpr(starter,expr_list,mids,ender) ->
	  (match expr_list with
	    [] -> failwith "bad disjunction"
	  | x::xs ->
	      bind (string_mcode starter)
		(bind (expression x)
		   (bind
		      (multibind
			 (List.map2
			    (function mid ->
			      function x ->
				bind (string_mcode mid) (expression x))
			    mids xs))
	              (string_mcode ender))))
      | Ast0.NestExpr(starter,expr_dots,ender,whencode,multi) ->
	  bind (string_mcode starter)
	    (bind (expression_dots expr_dots)
	       (bind (string_mcode ender) (get_option expression whencode)))
      | Ast0.Edots(dots,whencode) | Ast0.Ecircles(dots,whencode)
      | Ast0.Estars(dots,whencode) ->
	  bind (string_mcode dots) (get_option expression whencode)
      | Ast0.OptExp(exp) -> expression exp
      | Ast0.UniqueExp(exp) -> expression exp in
    exprfn all_functions k e
  and function_pointer (ty,lp1,star,rp1,lp2,params,rp2) extra =
    (* have to put the treatment of the identifier into the right position *)
    multibind
      ([typeC ty; string_mcode lp1; string_mcode star] @ extra @
       [string_mcode rp1;
	 string_mcode lp2; parameter_dots params; string_mcode rp2])
  and function_type (ty,lp1,params,rp1) extra =
    (* have to put the treatment of the identifier into the right position *)
    multibind ([get_option typeC ty] @ extra @
	       [string_mcode lp1; parameter_dots params; string_mcode rp1])
  and array_type (ty,lb,size,rb) extra =
    multibind
      ([typeC ty] @ extra @
       [string_mcode lb; get_option expression size; string_mcode rb])
  and typeC t =
    let k t =
      match Ast0.unwrap t with
	Ast0.ConstVol(cv,ty) -> bind (cv_mcode cv) (typeC ty)
      |	Ast0.BaseType(ty,strings) -> multibind (List.map string_mcode strings)
      |	Ast0.Signed(sign,ty) -> bind (sign_mcode sign) (get_option typeC ty)
      | Ast0.Pointer(ty,star) -> bind (typeC ty) (string_mcode star)
      | Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
	  function_pointer (ty,lp1,star,rp1,lp2,params,rp2) []
      | Ast0.FunctionType(ty,lp1,params,rp1) ->
	  function_type (ty,lp1,params,rp1) []
      | Ast0.Array(ty,lb,size,rb) ->
	  array_type (ty,lb,size,rb) []
      | Ast0.EnumName(kind,name) -> bind (string_mcode kind) (ident name)
      | Ast0.StructUnionName(kind,name) ->
	  bind (struct_mcode kind) (get_option ident name)
      | Ast0.StructUnionDef(ty,lb,decls,rb) ->
	  multibind
	    [typeC ty;string_mcode lb;declaration_dots decls;string_mcode rb]
      | Ast0.TypeName(name) -> string_mcode name
      | Ast0.MetaType(name,_) -> meta_mcode name
      |	Ast0.DisjType(starter,types,mids,ender) ->
	  (match types with
	    [] -> failwith "bad disjunction"
	  | x::xs ->
	      bind (string_mcode starter)
		(bind (typeC x)
		   (bind
		      (multibind
			 (List.map2
			    (function mid ->
			      function x ->
				bind (string_mcode mid) (typeC x))
			    mids xs))
	              (string_mcode ender))))
      | Ast0.OptType(ty) -> typeC ty
      | Ast0.UniqueType(ty) -> typeC ty in
    tyfn all_functions k t
  and named_type ty id =
    match Ast0.unwrap ty with
      Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
	function_pointer (ty,lp1,star,rp1,lp2,params,rp2) [ident id]
    | Ast0.FunctionType(ty,lp1,params,rp1) ->
	function_type (ty,lp1,params,rp1) [ident id]
    | Ast0.Array(ty,lb,size,rb) ->
	array_type (ty,lb,size,rb) [ident id]
    | _ -> bind (typeC ty) (ident id)
  and declaration d =
    let k d =
      match Ast0.unwrap d with
	Ast0.Init(stg,ty,id,eq,ini,sem) ->
	  bind (get_option storage_mcode stg)
	    (bind (named_type ty id)
	       (multibind
		  [string_mcode eq; initialiser ini; string_mcode sem]))
      | Ast0.UnInit(stg,ty,id,sem) ->
	  bind (get_option storage_mcode stg)
	    (bind (named_type ty id) (string_mcode sem))
      | Ast0.MacroDecl(name,lp,args,rp,sem) ->
	  multibind
	    [ident name; string_mcode lp; expression_dots args;
	      string_mcode rp; string_mcode sem]
      | Ast0.TyDecl(ty,sem) -> bind (typeC ty) (string_mcode sem)
      | Ast0.Typedef(stg,ty,id,sem) ->
	  bind (string_mcode stg)
	    (bind (typeC ty) (bind (typeC id) (string_mcode sem)))
      |	Ast0.DisjDecl(starter,decls,mids,ender) ->
	  (match decls with
	    [] -> failwith "bad disjunction"
	  | x::xs ->
	      bind (string_mcode starter)
		(bind (declaration x)
		   (bind
		      (multibind
			 (List.map2
			    (function mid ->
			      function x ->
				bind (string_mcode mid) (declaration x))
			    mids xs))
	              (string_mcode ender))))
      |	Ast0.Ddots(dots,whencode) ->
	  bind (string_mcode dots) (get_option declaration whencode)
      | Ast0.OptDecl(decl) -> declaration decl
      | Ast0.UniqueDecl(decl) -> declaration decl in
    declfn all_functions k d
  and initialiser i =
    let k i =
      match Ast0.unwrap i with
	Ast0.MetaInit(name,_) -> meta_mcode name
      |	Ast0.InitExpr(exp) -> expression exp
      | Ast0.InitList(lb,initlist,rb) ->
	  multibind
	    [string_mcode lb; initialiser_dots initlist; string_mcode rb]
      | Ast0.InitGccExt(designators,eq,ini) ->
	  multibind
	    ((List.map designator designators) @
	     [string_mcode eq; initialiser ini])
      | Ast0.InitGccName(name,eq,ini) ->
	  multibind [ident name; string_mcode eq; initialiser ini]
      | Ast0.IComma(cm) -> string_mcode cm
      | Ast0.Idots(dots,whencode) ->
	  bind (string_mcode dots) (get_option initialiser whencode)
      | Ast0.OptIni(i) -> initialiser i
      | Ast0.UniqueIni(i) -> initialiser i in
    initfn all_functions k i

  and designator = function
      Ast0.DesignatorField(dot,id) -> bind (string_mcode dot) (ident id)
    | Ast0.DesignatorIndex(lb,exp,rb) ->
	bind (string_mcode lb) (bind (expression exp) (string_mcode rb))
    | Ast0.DesignatorRange(lb,min,dots,max,rb) ->
	multibind
	  [string_mcode lb; expression min; string_mcode dots;
	    expression max; string_mcode rb]

  and parameterTypeDef p =
    let k p =
      match Ast0.unwrap p with
	Ast0.VoidParam(ty) -> typeC ty
      | Ast0.Param(ty,Some id) -> named_type ty id
      | Ast0.Param(ty,None) -> typeC ty
      | Ast0.MetaParam(name,_) -> meta_mcode name
      | Ast0.MetaParamList(name,_,_) -> meta_mcode name
      | Ast0.PComma(cm) -> string_mcode cm
      | Ast0.Pdots(dots) -> string_mcode dots
      | Ast0.Pcircles(dots) -> string_mcode dots
      | Ast0.OptParam(param) -> parameterTypeDef param
      | Ast0.UniqueParam(param) -> parameterTypeDef param in
    paramfn all_functions k p

  (* discard the result, because the statement is assumed to be already
     represented elsewhere in the code *)
  and process_bef_aft s =
    match Ast0.get_dots_bef_aft s with
      Ast0.NoDots -> ()
    | Ast0.DroppingBetweenDots(stm) -> let _ = statement stm in ()
    | Ast0.AddingBetweenDots(stm) -> let _ = statement stm in ()

  and statement s =
    process_bef_aft s;
    let k s =
      match Ast0.unwrap s with
	Ast0.FunDecl(_,fi,name,lp,params,rp,lbrace,body,rbrace) ->
	  multibind
	    ((List.map fninfo fi) @
	     [ident name; string_mcode lp;
	       parameter_dots params; string_mcode rp; string_mcode lbrace;
	       statement_dots body; string_mcode rbrace])
      | Ast0.Decl(_,decl) -> declaration decl
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
      | Ast0.Iterator(nm,lp,args,rp,body,_) ->
	  multibind
	    [ident nm; string_mcode lp; expression_dots args;
	      string_mcode rp; statement body]
      |	Ast0.Switch(switch,lp,exp,rp,lb,cases,rb) ->
	  multibind
	    [string_mcode switch; string_mcode lp; expression exp;
	      string_mcode rp; string_mcode lb; case_line_dots cases;
	      string_mcode rb]
      | Ast0.Break(br,sem) -> bind (string_mcode br) (string_mcode sem)
      | Ast0.Continue(cont,sem) -> bind (string_mcode cont) (string_mcode sem)
      |	Ast0.Label(l,dd) -> bind (ident l) (string_mcode dd)
      |	Ast0.Goto(goto,l,sem) ->
	  bind (string_mcode goto) (bind (ident l) (string_mcode sem))
      | Ast0.Return(ret,sem) -> bind (string_mcode ret) (string_mcode sem)
      | Ast0.ReturnExpr(ret,exp,sem) ->
	  multibind [string_mcode ret; expression exp; string_mcode sem]
      | Ast0.MetaStmt(name,_) -> meta_mcode name
      | Ast0.MetaStmtList(name,_) -> meta_mcode name
      | Ast0.Disj(starter,statement_dots_list,mids,ender) ->
	  (match statement_dots_list with
	    [] -> failwith "bad disjunction"
	  | x::xs ->
	      bind (string_mcode starter)
		(bind (statement_dots x)
		   (bind
		      (multibind
			 (List.map2
			    (function mid ->
			      function x ->
				bind (string_mcode mid) (statement_dots x))
			    mids xs))
	              (string_mcode ender))))
      | Ast0.Nest(starter,stmt_dots,ender,whn,multi) ->
	  bind (string_mcode starter)
	    (bind (statement_dots stmt_dots)
	       (bind (string_mcode ender)
		  (multibind
		     (List.map (whencode statement_dots statement) whn))))
      | Ast0.Exp(exp) -> expression exp
      | Ast0.TopExp(exp) -> expression exp
      | Ast0.Ty(ty) -> typeC ty
      | Ast0.TopInit(init) -> initialiser init
      | Ast0.Dots(d,whn) | Ast0.Circles(d,whn) | Ast0.Stars(d,whn) ->
	  bind (string_mcode d)
	    (multibind (List.map (whencode statement_dots statement) whn))
      | Ast0.Include(inc,name) -> bind (string_mcode inc) (inc_mcode name)
      | Ast0.Define(def,id,params,body) ->
	  multibind [string_mcode def; ident id; define_parameters params;
		      statement_dots body]
      | Ast0.OptStm(re) -> statement re
      | Ast0.UniqueStm(re) -> statement re in
    stmtfn all_functions k s

  (* not parameterizable for now... *)
  and define_parameters p =
    let k p =
      match Ast0.unwrap p with
	Ast0.NoParams -> option_default
      | Ast0.DParams(lp,params,rp) ->
	  multibind
	    [string_mcode lp; define_param_dots params; string_mcode rp] in
    k p

  and define_param_dots d =
    let k d =
      match Ast0.unwrap d with
	Ast0.DOTS(l) | Ast0.CIRCLES(l) | Ast0.STARS(l) ->
	  multibind (List.map define_param l) in
    k d

  and define_param p =
    let k p =
      match Ast0.unwrap p with
	Ast0.DParam(id) -> ident id
      | Ast0.DPComma(comma) -> string_mcode comma
      | Ast0.DPdots(d) -> string_mcode d
      | Ast0.DPcircles(c) -> string_mcode c
      | Ast0.OptDParam(dp) -> define_param dp
      | Ast0.UniqueDParam(dp) -> define_param dp in
    k p

  and fninfo = function
      Ast0.FStorage(stg) -> storage_mcode stg
    | Ast0.FType(ty) -> typeC ty
    | Ast0.FInline(inline) -> string_mcode inline
    | Ast0.FAttr(init) -> string_mcode init

  and whencode notfn alwaysfn = function
      Ast0.WhenNot a -> notfn a
    | Ast0.WhenAlways a -> alwaysfn a
    | Ast0.WhenModifier(_) -> option_default
    | Ast0.WhenNotTrue(e) -> expression e
    | Ast0.WhenNotFalse(e) -> expression e

  and case_line c =
    let k c =
      match Ast0.unwrap c with
	Ast0.Default(def,colon,code) ->
	  multibind [string_mcode def;string_mcode colon;statement_dots code]
      | Ast0.Case(case,exp,colon,code) ->
	  multibind [string_mcode case;expression exp;string_mcode colon;
		      statement_dots code]
      |	Ast0.OptCase(case) -> case_line case in
    casefn all_functions k c

  and anything a = (* for compile_iso, not parameterisable *)
    let k = function
	Ast0.DotsExprTag(exprs) -> expression_dots exprs
      | Ast0.DotsInitTag(inits) -> initialiser_dots inits
      | Ast0.DotsParamTag(params) -> parameter_dots params
      | Ast0.DotsStmtTag(stmts) -> statement_dots stmts
      | Ast0.DotsDeclTag(decls) -> declaration_dots decls
      | Ast0.DotsCaseTag(cases) -> case_line_dots cases
      | Ast0.IdentTag(id) -> ident id
      | Ast0.ExprTag(exp) -> expression exp
      | Ast0.ArgExprTag(exp) -> expression exp
      | Ast0.TestExprTag(exp) -> expression exp
      | Ast0.TypeCTag(ty) -> typeC ty
      | Ast0.ParamTag(param) -> parameterTypeDef param
      | Ast0.InitTag(init) -> initialiser init
      | Ast0.DeclTag(decl) -> declaration decl
      | Ast0.StmtTag(stmt) -> statement stmt
      | Ast0.CaseLineTag(c) -> case_line c
      | Ast0.TopTag(top) -> top_level top
      | Ast0.IsoWhenTag(_) -> option_default
      | Ast0.IsoWhenTTag(e) -> expression e
      | Ast0.IsoWhenFTag(e) -> expression e
      |	Ast0.MetaPosTag(var) -> failwith "not supported" in
    k a

  and top_level t =
    let k t =
      match Ast0.unwrap t with
	Ast0.FILEINFO(old_file,new_file) ->
	  bind (string_mcode old_file) (string_mcode new_file)
      | Ast0.DECL(stmt_dots) -> statement stmt_dots
      | Ast0.CODE(stmt_dots) -> statement_dots stmt_dots
      | Ast0.ERRORWORDS(exps) -> multibind (List.map expression exps)
      | Ast0.OTHER(_) -> failwith "unexpected code" in
    topfn all_functions k t
  and all_functions =
    {combiner_ident = ident;
      combiner_expression = expression;
      combiner_typeC = typeC;
      combiner_declaration = declaration;
      combiner_initialiser = initialiser;
      combiner_initialiser_list = initialiser_dots;
      combiner_parameter = parameterTypeDef;
      combiner_parameter_list = parameter_dots;
      combiner_statement = statement;
      combiner_case_line = case_line;
      combiner_top_level = top_level;
      combiner_expression_dots = expression_dots;
      combiner_statement_dots = statement_dots;
      combiner_declaration_dots = declaration_dots;
      combiner_case_line_dots = case_line_dots;
      combiner_anything = anything} in
  all_functions

(* --------------------------------------------------------------------- *)
(* Generic traversal: rebuilder *)

type 'a inout = 'a -> 'a (* for specifying the type of rebuilder *)

type rebuilder =
    {rebuilder_ident : Ast0.ident inout;
      rebuilder_expression : Ast0.expression inout;
      rebuilder_typeC : Ast0.typeC inout;
      rebuilder_declaration : Ast0.declaration inout;
      rebuilder_initialiser : Ast0.initialiser inout;
      rebuilder_initialiser_list : Ast0.initialiser_list inout;
      rebuilder_parameter : Ast0.parameterTypeDef inout;
      rebuilder_parameter_list : Ast0.parameter_list inout;
      rebuilder_statement : Ast0.statement inout;
      rebuilder_case_line : Ast0.case_line inout;
      rebuilder_top_level : Ast0.top_level inout;
      rebuilder_expression_dots :
	Ast0.expression Ast0.dots ->
	  Ast0.expression Ast0.dots;
	  rebuilder_statement_dots :
	    Ast0.statement Ast0.dots ->
	      Ast0.statement Ast0.dots;
	  rebuilder_declaration_dots :
	    Ast0.declaration Ast0.dots ->
	      Ast0.declaration Ast0.dots;
	  rebuilder_case_line_dots :
	    Ast0.case_line Ast0.dots ->
	      Ast0.case_line Ast0.dots;
	  rebuilder_anything :
	    Ast0.anything -> Ast0.anything}

type 'mc rmcode = 'mc Ast0.mcode inout
type 'cd rcode = rebuilder -> ('cd inout) -> 'cd inout

let rebuilder = fun
    meta_mcode string_mcode const_mcode assign_mcode fix_mcode unary_mcode
    binary_mcode cv_mcode sign_mcode struct_mcode storage_mcode
    inc_mcode
    dotsexprfn dotsinitfn dotsparamfn dotsstmtfn dotsdeclfn dotscasefn
    identfn exprfn tyfn initfn paramfn declfn stmtfn casefn topfn ->
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
  and initialiser_list i =
    let k i =
      Ast0.rewrap i
	(match Ast0.unwrap i with
	  Ast0.DOTS(l) -> Ast0.DOTS(List.map initialiser l)
	| Ast0.CIRCLES(l) -> Ast0.CIRCLES(List.map initialiser l)
	| Ast0.STARS(l) -> Ast0.STARS(List.map initialiser l)) in
    dotsinitfn all_functions k i
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
  and declaration_dots d =
    let k d =
      Ast0.rewrap d
	(match Ast0.unwrap d with
	  Ast0.DOTS(l) -> Ast0.DOTS(List.map declaration l)
	| Ast0.CIRCLES(l) -> Ast0.CIRCLES(List.map declaration l)
	| Ast0.STARS(l) -> Ast0.STARS(List.map declaration l)) in
    dotsdeclfn all_functions k d
  and case_line_dots d =
    let k d =
      Ast0.rewrap d
	(match Ast0.unwrap d with
	  Ast0.DOTS(l) -> Ast0.DOTS(List.map case_line l)
	| Ast0.CIRCLES(l) -> Ast0.CIRCLES(List.map case_line l)
	| Ast0.STARS(l) -> Ast0.STARS(List.map case_line l)) in
    dotscasefn all_functions k d
  and ident i =
    let k i =
      Ast0.rewrap i
	(match Ast0.unwrap i with
	  Ast0.Id(name) -> Ast0.Id(string_mcode name)
	| Ast0.MetaId(name,constraints,pure) ->
	    Ast0.MetaId(meta_mcode name,constraints,pure)
	| Ast0.MetaFunc(name,constraints,pure) ->
	    Ast0.MetaFunc(meta_mcode name,constraints,pure)
	| Ast0.MetaLocalFunc(name,constraints,pure) ->
	    Ast0.MetaLocalFunc(meta_mcode name,constraints,pure)
	| Ast0.OptIdent(id) -> Ast0.OptIdent(ident id)
	| Ast0.UniqueIdent(id) -> Ast0.UniqueIdent(ident id)) in
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
	| Ast0.Assignment(left,op,right,simple) ->
	    Ast0.Assignment(expression left,assign_mcode op,expression right,
			    simple)
	| Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
	    Ast0.CondExpr(expression exp1, string_mcode why,
			  get_option expression exp2, string_mcode colon,
			  expression exp3)
	| Ast0.Postfix(exp,op) -> Ast0.Postfix(expression exp, fix_mcode op)
	| Ast0.Infix(exp,op) -> Ast0.Infix(expression exp, fix_mcode op)
	| Ast0.Unary(exp,op) -> Ast0.Unary(expression exp, unary_mcode op)
	| Ast0.Binary(left,op,right) ->
	    Ast0.Binary(expression left, binary_mcode op, expression right)
	| Ast0.Nested(left,op,right) ->
	    Ast0.Nested(expression left, binary_mcode op, expression right)
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
	| Ast0.TypeExp(ty) -> Ast0.TypeExp(typeC ty)
	| Ast0.MetaErr(name,constraints,pure) ->
	    Ast0.MetaErr(meta_mcode name,constraints,pure)
	| Ast0.MetaExpr(name,constraints,ty,form,pure) ->
	    Ast0.MetaExpr(meta_mcode name,constraints,ty,form,pure)
	| Ast0.MetaExprList(name,lenname,pure) ->
	    Ast0.MetaExprList(meta_mcode name,lenname,pure)
	| Ast0.EComma(cm) -> Ast0.EComma(string_mcode cm)
	| Ast0.DisjExpr(starter,expr_list,mids,ender) ->
	    Ast0.DisjExpr(string_mcode starter,List.map expression expr_list,
			  List.map string_mcode mids,string_mcode ender)
	| Ast0.NestExpr(starter,expr_dots,ender,whencode,multi) ->
	    Ast0.NestExpr(string_mcode starter,expression_dots expr_dots,
			  string_mcode ender, get_option expression whencode,
			  multi)
	| Ast0.Edots(dots,whencode) ->
	    Ast0.Edots(string_mcode dots, get_option expression whencode)
	| Ast0.Ecircles(dots,whencode) ->
	    Ast0.Ecircles(string_mcode dots, get_option expression whencode)
	| Ast0.Estars(dots,whencode) ->
	    Ast0.Estars(string_mcode dots, get_option expression whencode)
	| Ast0.OptExp(exp) -> Ast0.OptExp(expression exp)
	| Ast0.UniqueExp(exp) -> Ast0.UniqueExp(expression exp)) in
    exprfn all_functions k e
  and typeC t =
    let k t =
      Ast0.rewrap t
	(match Ast0.unwrap t with
	  Ast0.ConstVol(cv,ty) -> Ast0.ConstVol(cv_mcode cv,typeC ty)
	| Ast0.BaseType(ty,strings) ->
	    Ast0.BaseType(ty, List.map string_mcode strings)
	| Ast0.Signed(sign,ty) ->
	    Ast0.Signed(sign_mcode sign,get_option typeC ty)
	| Ast0.Pointer(ty,star) ->
	    Ast0.Pointer(typeC ty, string_mcode star)
	| Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
	    Ast0.FunctionPointer(typeC ty,string_mcode lp1,string_mcode star,
				 string_mcode rp1,string_mcode lp2,
				 parameter_list params,
				 string_mcode rp2)
	| Ast0.FunctionType(ty,lp1,params,rp1) ->
	    Ast0.FunctionType(get_option typeC ty,
			      string_mcode lp1,parameter_list params,
			      string_mcode rp1)
	| Ast0.Array(ty,lb,size,rb) ->
	    Ast0.Array(typeC ty, string_mcode lb,
		       get_option expression size, string_mcode rb)
	| Ast0.EnumName(kind,name) ->
	    Ast0.EnumName(string_mcode kind, ident name)
	| Ast0.StructUnionName(kind,name) ->
	    Ast0.StructUnionName (struct_mcode kind, get_option ident name)
	| Ast0.StructUnionDef(ty,lb,decls,rb) ->
	    Ast0.StructUnionDef (typeC ty,
				 string_mcode lb, declaration_dots decls,
				 string_mcode rb)
	| Ast0.TypeName(name) -> Ast0.TypeName(string_mcode name)
	| Ast0.MetaType(name,pure) ->
	    Ast0.MetaType(meta_mcode name,pure)
	| Ast0.DisjType(starter,types,mids,ender) ->
	    Ast0.DisjType(string_mcode starter,List.map typeC types,
			  List.map string_mcode mids,string_mcode ender)
	| Ast0.OptType(ty) -> Ast0.OptType(typeC ty)
	| Ast0.UniqueType(ty) -> Ast0.UniqueType(typeC ty)) in
    tyfn all_functions k t
  and declaration d =
    let k d =
      Ast0.rewrap d
	(match Ast0.unwrap d with
	  Ast0.Init(stg,ty,id,eq,ini,sem) ->
	    Ast0.Init(get_option storage_mcode stg,
		      typeC ty, ident id, string_mcode eq, initialiser ini,
		      string_mcode sem)
	| Ast0.UnInit(stg,ty,id,sem) ->
	    Ast0.UnInit(get_option storage_mcode stg,
			typeC ty, ident id, string_mcode sem)
	| Ast0.MacroDecl(name,lp,args,rp,sem) ->
	    Ast0.MacroDecl(ident name,string_mcode lp,
			   expression_dots args,
			   string_mcode rp,string_mcode sem)
	| Ast0.TyDecl(ty,sem) -> Ast0.TyDecl(typeC ty, string_mcode sem)
	| Ast0.Typedef(stg,ty,id,sem) ->
	    Ast0.Typedef(string_mcode stg, typeC ty, typeC id,
			 string_mcode sem)
	| Ast0.DisjDecl(starter,decls,mids,ender) ->
	    Ast0.DisjDecl(string_mcode starter,List.map declaration decls,
			  List.map string_mcode mids,string_mcode ender)
	| Ast0.Ddots(dots,whencode) ->
	    Ast0.Ddots(string_mcode dots, get_option declaration whencode)
	| Ast0.OptDecl(decl) -> Ast0.OptDecl(declaration decl)
	| Ast0.UniqueDecl(decl) -> Ast0.UniqueDecl(declaration decl)) in
    declfn all_functions k d
  and initialiser i =
    let k i =
      Ast0.rewrap i
	(match Ast0.unwrap i with
	  Ast0.MetaInit(name,pure) ->
	    Ast0.MetaInit(meta_mcode name,pure)
	| Ast0.InitExpr(exp) -> Ast0.InitExpr(expression exp)
	| Ast0.InitList(lb,initlist,rb) ->
	    Ast0.InitList(string_mcode lb, initialiser_list initlist,
			  string_mcode rb)
	| Ast0.InitGccExt(designators,eq,ini) ->
	    Ast0.InitGccExt
	      (List.map designator designators, string_mcode eq,
	       initialiser ini)
	| Ast0.InitGccName(name,eq,ini) ->
	    Ast0.InitGccName(ident name, string_mcode eq, initialiser ini)
	| Ast0.IComma(cm) -> Ast0.IComma(string_mcode cm)
	| Ast0.Idots(d,whencode) ->
	    Ast0.Idots(string_mcode d, get_option initialiser whencode)
	| Ast0.OptIni(i) -> Ast0.OptIni(initialiser i)
	| Ast0.UniqueIni(i) -> Ast0.UniqueIni(initialiser i)) in
    initfn all_functions k i

  and designator = function
      Ast0.DesignatorField(dot,id) ->
	Ast0.DesignatorField(string_mcode dot,ident id)
    | Ast0.DesignatorIndex(lb,exp,rb) ->
	Ast0.DesignatorIndex(string_mcode lb,expression exp,string_mcode rb)
    | Ast0.DesignatorRange(lb,min,dots,max,rb) ->
	Ast0.DesignatorRange(string_mcode lb,expression min,string_mcode dots,
			     expression max,string_mcode rb)

  and parameterTypeDef p =
    let k p =
      Ast0.rewrap p
	(match Ast0.unwrap p with
	  Ast0.VoidParam(ty) -> Ast0.VoidParam(typeC ty)
	| Ast0.Param(ty,id) -> Ast0.Param(typeC ty, get_option ident id)
	| Ast0.MetaParam(name,pure) ->
	    Ast0.MetaParam(meta_mcode name,pure)
	| Ast0.MetaParamList(name,lenname,pure) ->
	    Ast0.MetaParamList(meta_mcode name,lenname,pure)
	| Ast0.PComma(cm) -> Ast0.PComma(string_mcode cm)
	| Ast0.Pdots(dots) -> Ast0.Pdots(string_mcode dots)
	| Ast0.Pcircles(dots) -> Ast0.Pcircles(string_mcode dots)
	| Ast0.OptParam(param) -> Ast0.OptParam(parameterTypeDef param)
	| Ast0.UniqueParam(param) ->
	    Ast0.UniqueParam(parameterTypeDef param)) in
    paramfn all_functions k p
  (* not done for combiner, because the statement is assumed to be already
     represented elsewhere in the code *)
  and process_bef_aft s =
    Ast0.set_dots_bef_aft s
      (match Ast0.get_dots_bef_aft s with
	Ast0.NoDots -> Ast0.NoDots
      | Ast0.DroppingBetweenDots(stm) ->
	  Ast0.DroppingBetweenDots(statement stm)
      | Ast0.AddingBetweenDots(stm) ->
	  Ast0.AddingBetweenDots(statement stm))

  and statement s =
    let k s =
      Ast0.rewrap s
	(match Ast0.unwrap s with
	  Ast0.FunDecl(bef,fi,name,lp,params,rp,lbrace,body,rbrace) ->
	    Ast0.FunDecl(bef,List.map fninfo fi, ident name,
			 string_mcode lp, parameter_list params,
			 string_mcode rp, string_mcode lbrace,
			 statement_dots body, string_mcode rbrace)
	| Ast0.Decl(bef,decl) -> Ast0.Decl(bef,declaration decl)
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
	| Ast0.Iterator(nm,lp,args,rp,body,aft) ->
	    Ast0.Iterator(ident nm, string_mcode lp,
			  expression_dots args,
			  string_mcode rp, statement body, aft)
	| Ast0.Switch(switch,lp,exp,rp,lb,cases,rb) ->
      	    Ast0.Switch(string_mcode switch,string_mcode lp,expression exp,
			string_mcode rp,string_mcode lb,
			case_line_dots cases, string_mcode rb)
	| Ast0.Break(br,sem) ->
	    Ast0.Break(string_mcode br,string_mcode sem)
	| Ast0.Continue(cont,sem) ->
	    Ast0.Continue(string_mcode cont,string_mcode sem)
	| Ast0.Label(l,dd) -> Ast0.Label(ident l,string_mcode dd)
	| Ast0.Goto(goto,l,sem) ->
	    Ast0.Goto(string_mcode goto,ident l,string_mcode sem)
	| Ast0.Return(ret,sem) ->
	    Ast0.Return(string_mcode ret,string_mcode sem)
	| Ast0.ReturnExpr(ret,exp,sem) ->
	    Ast0.ReturnExpr(string_mcode ret,expression exp,string_mcode sem)
	| Ast0.MetaStmt(name,pure) ->
	    Ast0.MetaStmt(meta_mcode name,pure)
	| Ast0.MetaStmtList(name,pure) ->
	    Ast0.MetaStmtList(meta_mcode name,pure)
	| Ast0.Disj(starter,statement_dots_list,mids,ender) ->
	    Ast0.Disj(string_mcode starter,
		      List.map statement_dots statement_dots_list,
		      List.map string_mcode mids,
		      string_mcode ender)
	| Ast0.Nest(starter,stmt_dots,ender,whn,multi) ->
	    Ast0.Nest(string_mcode starter,statement_dots stmt_dots,
		      string_mcode ender,
		      List.map (whencode statement_dots statement) whn,
		      multi)
	| Ast0.Exp(exp) -> Ast0.Exp(expression exp)
	| Ast0.TopExp(exp) -> Ast0.TopExp(expression exp)
	| Ast0.Ty(ty) -> Ast0.Ty(typeC ty)
	| Ast0.TopInit(init) -> Ast0.TopInit(initialiser init)
	| Ast0.Dots(d,whn) ->
	    Ast0.Dots(string_mcode d,
		      List.map (whencode statement_dots statement) whn)
	| Ast0.Circles(d,whn) ->
	    Ast0.Circles(string_mcode d,
			 List.map (whencode statement_dots statement) whn)
	| Ast0.Stars(d,whn) ->
	    Ast0.Stars(string_mcode d,
		       List.map (whencode statement_dots statement) whn)
	| Ast0.Include(inc,name) ->
	    Ast0.Include(string_mcode inc,inc_mcode name)
	| Ast0.Define(def,id,params,body) ->
	    Ast0.Define(string_mcode def,ident id,
			define_parameters params,
			statement_dots body)
	| Ast0.OptStm(re) -> Ast0.OptStm(statement re)
	| Ast0.UniqueStm(re) -> Ast0.UniqueStm(statement re)) in
    let s = stmtfn all_functions k s in
    process_bef_aft s

  (* not parameterizable for now... *)
  and define_parameters p =
    let k p =
      Ast0.rewrap p
	(match Ast0.unwrap p with
	  Ast0.NoParams -> Ast0.NoParams
	| Ast0.DParams(lp,params,rp) ->
	    Ast0.DParams(string_mcode lp,define_param_dots params,
			 string_mcode rp))in
    k p

  and define_param_dots d =
    let k d =
      Ast0.rewrap d
	(match Ast0.unwrap d with
	  Ast0.DOTS(l) -> Ast0.DOTS(List.map define_param l)
	| Ast0.CIRCLES(l) -> Ast0.CIRCLES(List.map define_param l)
	| Ast0.STARS(l) -> Ast0.STARS(List.map define_param l)) in
    k d

  and define_param p =
    let k p =
      Ast0.rewrap p
	(match Ast0.unwrap p with
	  Ast0.DParam(id) -> Ast0.DParam(ident id)
	| Ast0.DPComma(comma) -> Ast0.DPComma(string_mcode comma)
	| Ast0.DPdots(d) -> Ast0.DPdots(string_mcode d)
	| Ast0.DPcircles(c) -> Ast0.DPcircles(string_mcode c)
	| Ast0.OptDParam(dp) -> Ast0.OptDParam(define_param dp)
	| Ast0.UniqueDParam(dp) -> Ast0.UniqueDParam(define_param dp)) in
    k p

  and fninfo = function
      Ast0.FStorage(stg) -> Ast0.FStorage(storage_mcode stg)
    | Ast0.FType(ty) -> Ast0.FType(typeC ty)
    | Ast0.FInline(inline) -> Ast0.FInline(string_mcode inline)
    | Ast0.FAttr(init) -> Ast0.FAttr(string_mcode init)

  and whencode notfn alwaysfn = function
      Ast0.WhenNot a -> Ast0.WhenNot (notfn a)
    | Ast0.WhenAlways a -> Ast0.WhenAlways (alwaysfn a)
    | Ast0.WhenModifier(x) -> Ast0.WhenModifier(x)
    | Ast0.WhenNotTrue(e) -> Ast0.WhenNotTrue(expression e)
    | Ast0.WhenNotFalse(e) -> Ast0.WhenNotFalse(expression e)

  and case_line c =
    let k c =
      Ast0.rewrap c
	(match Ast0.unwrap c with
	  Ast0.Default(def,colon,code) ->
	    Ast0.Default(string_mcode def,string_mcode colon,
			 statement_dots code)
	| Ast0.Case(case,exp,colon,code) ->
	    Ast0.Case(string_mcode case,expression exp,string_mcode colon,
		      statement_dots code)
	| Ast0.OptCase(case) -> Ast0.OptCase(case_line case)) in
    casefn all_functions k c

  and top_level t =
    let k t =
      Ast0.rewrap t
	(match Ast0.unwrap t with
	  Ast0.FILEINFO(old_file,new_file) ->
	    Ast0.FILEINFO(string_mcode old_file, string_mcode new_file)
	| Ast0.DECL(statement_dots) ->
	    Ast0.DECL(statement statement_dots)
	| Ast0.CODE(stmt_dots) -> Ast0.CODE(statement_dots stmt_dots)
	| Ast0.ERRORWORDS(exps) -> Ast0.ERRORWORDS(List.map expression exps)
	| Ast0.OTHER(_) -> failwith "unexpected code") in
    topfn all_functions k t

  and anything a = (* for compile_iso, not parameterisable *)
    let k = function
	Ast0.DotsExprTag(exprs) -> Ast0.DotsExprTag(expression_dots exprs)
      | Ast0.DotsInitTag(inits) -> Ast0.DotsInitTag(initialiser_list inits)
      | Ast0.DotsParamTag(params) -> Ast0.DotsParamTag(parameter_list params)
      | Ast0.DotsStmtTag(stmts) -> Ast0.DotsStmtTag(statement_dots stmts)
      | Ast0.DotsDeclTag(decls) -> Ast0.DotsDeclTag(declaration_dots decls)
      | Ast0.DotsCaseTag(cases) -> Ast0.DotsCaseTag(case_line_dots cases)
      | Ast0.IdentTag(id) -> Ast0.IdentTag(ident id)
      | Ast0.ExprTag(exp) -> Ast0.ExprTag(expression exp)
      | Ast0.ArgExprTag(exp) -> Ast0.ArgExprTag(expression exp)
      | Ast0.TestExprTag(exp) -> Ast0.TestExprTag(expression exp)
      | Ast0.TypeCTag(ty) -> Ast0.TypeCTag(typeC ty)
      | Ast0.ParamTag(param) -> Ast0.ParamTag(parameterTypeDef param)
      | Ast0.InitTag(init) -> Ast0.InitTag(initialiser init)
      | Ast0.DeclTag(decl) -> Ast0.DeclTag(declaration decl)
      | Ast0.StmtTag(stmt) -> Ast0.StmtTag(statement stmt)
      | Ast0.CaseLineTag(c) -> Ast0.CaseLineTag(case_line c)
      | Ast0.TopTag(top) -> Ast0.TopTag(top_level top)
      | Ast0.IsoWhenTag(x) -> Ast0.IsoWhenTag(x)
      | Ast0.IsoWhenTTag(e) -> Ast0.IsoWhenTTag(expression e)
      | Ast0.IsoWhenFTag(e) -> Ast0.IsoWhenFTag(expression e)
      |	Ast0.MetaPosTag(var) -> failwith "not supported" in
    k a

  (* not done for combiner, because the statement is assumed to be already
     represented elsewhere in the code *)

  and all_functions =
    {rebuilder_ident = ident;
      rebuilder_expression = expression;
      rebuilder_typeC = typeC;
      rebuilder_declaration = declaration;
      rebuilder_initialiser = initialiser;
      rebuilder_initialiser_list = initialiser_list;
      rebuilder_parameter = parameterTypeDef;
      rebuilder_parameter_list = parameter_list;
      rebuilder_statement = statement;
      rebuilder_case_line = case_line;
      rebuilder_top_level = top_level;
      rebuilder_expression_dots = expression_dots;
      rebuilder_statement_dots = statement_dots;
      rebuilder_declaration_dots = declaration_dots;
      rebuilder_case_line_dots = case_line_dots;
      rebuilder_anything = anything} in
  all_functions
