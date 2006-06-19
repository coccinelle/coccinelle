(* Arities matter for the minus slice, but not for the plus slice. *)

(* + only allowed on code in a nest (in_nest = true).  ? only allowed on
rule_elems, and on subterms if the context is ? also. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)

let get_option fn = function
    None -> None
  | Some x -> Some (fn x)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Mcode *)

let mcode(term,_,mcodekind) = (term,mcodekind)

(* --------------------------------------------------------------------- *)
(* Dots *)

let dots fn d =
  match Ast0.unwrap d with
    Ast0.DOTS(x) -> Ast.DOTS(List.map fn x)
  | Ast0.CIRCLES(x) -> Ast.CIRCLES(List.map fn x)
  | Ast0.STARS(x) -> Ast.STARS(List.map fn x)

(* --------------------------------------------------------------------- *)
(* Identifier *)


let rec ident = function
    Ast0.Id(name) -> Ast.Id(mcode name)
  | Ast0.MetaId(name) -> Ast.MetaId(mcode name)
  | Ast0.MetaFunc(name) -> Ast.MetaFunc(mcode name)
  | Ast0.MetaLocalFunc(name) -> Ast.MetaLocalFunc(mcode name)
  | Ast0.OptIdent(id) -> Ast.OptIdent(ident id)
  | Ast0.UniqueIdent(id) -> Ast.UniqueIdent(ident id)
  | Ast0.MultiIdent(id) -> Ast.MultiIdent(ident id)

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression e =
  match Ast0.unwrap e with
    Ast0.Ident(id) -> Ast.Ident(ident id)
  | Ast0.Constant(const) ->
      Ast.Constant(mcode const)
  | Ast0.FunCall(fn,lp,args,rp) ->
      let fn = expression fn in
      let lp = mcode lp in
      let args = dots expression args in
      let rp = mcode rp in
      Ast.FunCall(fn,lp,args,rp)
  | Ast0.Assignment(left,op,right) ->
      Ast.Assignment(expression left,mcode op,expression right)
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
      let exp1 = expression exp1 in
      let why = mcode why in
      let exp2 = get_option expression exp2 in
      let colon = mcode colon in
      let exp3 = expression exp3 in
      Ast.CondExpr(exp1,why,exp2,colon,exp3)
  | Ast0.Postfix(exp,op) ->
      Ast.Postfix(expression exp,mcode op)
  | Ast0.Infix(exp,op) ->
      Ast.Infix(expression exp,mcode op)
  | Ast0.Unary(exp,op) ->
      Ast.Unary(expression exp,mcode op)
  | Ast0.Binary(left,op,right) ->
      Ast.Binary(expression left,mcode op,expression right)
  | Ast0.Paren(lp,exp,rp) ->
      Ast.Paren(mcode lp,expression exp,mcode rp)
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
      Ast.ArrayAccess(expression exp1,mcode lb,expression exp2,mcode rb)
  | Ast0.RecordAccess(exp,pt,field) ->
      Ast.RecordAccess(expression exp,mcode pt,ident field)
  | Ast0.RecordPtAccess(exp,ar,field) ->
      Ast.RecordPtAccess(expression exp,mcode ar,ident field)
  | Ast0.Cast(lp,ty,rp,exp) ->
      Ast.Cast(mcode lp,fullType ty,mcode rp,expression exp)
  | Ast0.MetaConst(name,ty)  ->
      let name = mcode name in
      let ty = get_option (List.map fullType) ty in
      Ast.MetaConst(name,ty)
  | Ast0.MetaErr(name)  -> Ast.MetaErr(mcode name)
  | Ast0.MetaExpr(name,ty)  ->
      let name = mcode name in
      let ty = get_option (List.map fullType) ty in
      Ast.MetaExpr(name,ty)
  | Ast0.MetaExprList(name) -> Ast.MetaExprList(mcode name)
  | Ast0.EComma(cm)         -> Ast.EComma(mcode cm)
  | Ast0.DisjExpr(exps)     -> Ast.DisjExpr(List.map expression exps)
  | Ast0.NestExpr(exp_dots) -> Ast.NestExpr(dots expression exp_dots)
  | Ast0.Edots(dots,whencode) ->
      let dots = mcode dots in
      let whencode = get_option expression whencode in
      Ast.Edots(dots,whencode)
  | Ast0.Ecircles(dots,whencode) ->
      let dots = mcode dots in
      let whencode = get_option expression whencode in
      Ast.Ecircles(dots,whencode)
  | Ast0.Estars(dots,whencode) ->
      let dots = mcode dots in
      let whencode = get_option expression whencode in
      Ast.Estars(dots,whencode)
  | Ast0.OptExp(exp) -> Ast.OptExp(expression exp)
  | Ast0.UniqueExp(exp) -> Ast.UniqueExp(expression exp)
  | Ast0.MultiExp(exp) -> Ast.MultiExp(expression exp)

(* --------------------------------------------------------------------- *)
(* Types *)

and fullType ft =
  match Ast0.unwrap ft with
    Ast0.Type(cv,ty) -> Ast.Type(get_option mcode cv,typeC ty)
  | Ast0.OptType(ty) -> Ast.OptType(fullType ty)
  | Ast0.UniqueType(ty) -> Ast.UniqueType(fullType ty)
  | Ast0.MultiType(ty) -> Ast.MultiType(fullType ty)

and typeC t =
  match Ast0.unwrap t with
    Ast0.BaseType(ty,sign) -> Ast.BaseType(mcode ty,get_option mcode sign)
  | Ast0.Pointer(ty,star) -> Ast.Pointer(fullType ty,mcode star)
  | Ast0.Array(ty,lb,size,rb) ->
      Ast.Array(fullType ty,mcode lb,get_option expression size,mcode rb)
  | Ast0.StructUnionName(name,kind) ->
      Ast.StructUnionName(mcode name,mcode kind)
  | Ast0.TypeName(name) -> Ast.TypeName(mcode name)
  | Ast0.MetaType(name) -> Ast.MetaType(mcode name)

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

let rec declaration d =
  match Ast0.unwrap d with
    Ast0.Init(ty,id,eq,exp,sem) ->
      let ty = fullType ty in
      let id = ident id in
      let eq = mcode eq in
      let exp = expression exp in
      let sem = mcode sem in
      Ast.Init(ty,id,eq,exp,sem)
  | Ast0.UnInit(ty,id,sem) -> Ast.UnInit(fullType ty,ident id,mcode sem)
  | Ast0.OptDecl(decl) -> Ast.OptDecl(declaration decl)
  | Ast0.UniqueDecl(decl) -> Ast.UniqueDecl(declaration decl)
  | Ast0.MultiDecl(decl) -> Ast.MultiDecl(declaration decl)

(* --------------------------------------------------------------------- *)
(* Parameter *)

let rec parameterTypeDef p =
  match Ast0.unwrap p with
    Ast0.VoidParam(ty) -> Ast.VoidParam(fullType ty)
  | Ast0.Param(id,ty) -> Ast.Param(ident id,fullType ty)
  | Ast0.MetaParam(name) -> Ast.MetaParam(mcode name)
  | Ast0.MetaParamList(name) -> Ast.MetaParamList(mcode name)
  | Ast0.PComma(cm) -> Ast.PComma(mcode cm)
  | Ast0.Pdots(dots) -> Ast.Pdots(mcode dots)
  | Ast0.Pcircles(dots) -> Ast.Pcircles(mcode dots)
  | Ast0.OptParam(param) -> Ast.OptParam(parameterTypeDef param)
  | Ast0.UniqueParam(param) -> Ast.UniqueParam(parameterTypeDef param)

let parameter_list = dots parameterTypeDef

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rec statement s =
  match Ast0.unwrap s with
    Ast0.Decl(decl) -> Ast.Atomic(Ast.Decl(declaration decl))
  | Ast0.Seq(lbrace,body,rbrace) -> 
      let lbrace = mcode lbrace in
      let body = dots statement body in
      let rbrace = mcode rbrace in
      Ast.Seq(Ast.SeqStart(lbrace),body,Ast.SeqEnd(rbrace))
  | Ast0.ExprStatement(exp,sem) ->
      Ast.Atomic(Ast.ExprStatement(expression exp,mcode sem))
  | Ast0.IfThen(iff,lp,exp,rp,branch) ->
      Ast.IfThen(Ast.IfHeader(mcode iff,mcode lp,expression exp,mcode rp),
		 statement branch)
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
      Ast.IfThenElse(Ast.IfHeader(mcode iff,mcode lp,expression exp,mcode rp),
		     statement branch1,Ast.Else(mcode els), statement branch2)
  | Ast0.While(wh,lp,exp,rp,body) ->
      Ast.While(Ast.WhileHeader(mcode wh,mcode lp,expression exp,mcode rp),
		statement body)
  | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
      Ast.Do(Ast.DoHeader(mcode d), statement body,
	     Ast.WhileTail(mcode wh,mcode lp,expression exp,mcode rp,
			   mcode sem))
  | Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body) ->
      let fr = mcode fr in
      let lp = mcode lp in
      let exp1 = get_option expression exp1 in
      let sem1 = mcode sem1 in
      let exp2 = get_option expression exp2 in
      let sem2= mcode sem2 in
      let exp3 = get_option expression exp3 in
      let rp = mcode rp in
      let body = statement body in
      Ast.For(Ast.ForHeader(fr,lp,exp1,sem1,exp2,sem2,exp3,rp),body)
  | Ast0.Return(ret,sem) -> Ast.Atomic(Ast.Return(mcode ret,mcode sem))
  | Ast0.ReturnExpr(ret,exp,sem) ->
      Ast.Atomic(Ast.ReturnExpr(mcode ret,expression exp,mcode sem))
  | Ast0.MetaStmt(name) ->
      Ast.Atomic(Ast.MetaStmt(mcode name))
  | Ast0.MetaStmtList(name) ->
      Ast.Atomic(Ast.MetaStmtList(mcode name))
  | Ast0.Exp(exp) ->
      Ast.Atomic(Ast.Exp(expression exp))
  | Ast0.Disj(rule_elem_dots_list) ->
      Ast.Disj(List.map (function x -> dots statement x) rule_elem_dots_list)
  | Ast0.Nest(rule_elem_dots) ->
      Ast.Nest(dots statement rule_elem_dots)
  | Ast0.Dots(d,whencode) ->
      let d = mcode d in
      let whencode = get_option (dots statement) whencode in
      Ast.Dots(d,whencode,[])
  | Ast0.Circles(d,whencode) ->
      let d = mcode d in
      let whencode = get_option (dots statement) whencode in
      Ast.Circles(d,whencode,[])
  | Ast0.Stars(d,whencode) ->
      let d = mcode d in
      let whencode = get_option (dots statement) whencode in
      Ast.Stars(d,whencode,[])
  | Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace) ->
      let stg = get_option mcode stg in
      let name = ident name in
      let lp = mcode lp in
      let params = parameter_list params in
      let rp = mcode rp in
      let lbrace = mcode lbrace in
      let body = dots statement body in
      let rbrace = mcode rbrace in
      Ast.FunDecl(Ast.FunHeader(stg,name,lp,params,rp),
		  Ast.SeqStart(lbrace), body, Ast.SeqEnd(rbrace))
  | Ast0.OptStm(stm) -> Ast.OptStm(statement stm)
  | Ast0.UniqueStm(stm) -> Ast.UniqueStm(statement stm)
  | Ast0.MultiStm(stm) -> Ast.MultiStm(statement stm)

let statement_dots = dots statement
	
(* --------------------------------------------------------------------- *)
(* Function declaration *)
(* Haven't thought much about arity here... *)

let top_level = function
    Ast0.DECL(decl) -> Ast.DECL(declaration decl)
  | Ast0.INCLUDE(inc,s) -> Ast.INCLUDE(mcode inc,mcode s)
  | Ast0.FILEINFO(old_file,new_file) ->
      Ast.FILEINFO(mcode old_file,mcode new_file)
  | Ast0.FUNCTION(stmt) -> Ast.FUNCTION(statement stmt)
  | Ast0.CODE(rule_elem_dots) -> Ast.CODE(dots statement rule_elem_dots)
  | Ast0.ERRORWORDS(exps) -> Ast.ERRORWORDS(List.map expression exps)
  | Ast0.OTHER(_) -> failwith "eliminated by top_level"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let ast0toast = List.map top_level
