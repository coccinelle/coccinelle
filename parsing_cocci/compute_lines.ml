(* Computes starting and ending logical lines for statements and
expressions - this information might not be needed any more *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
    
(* --------------------------------------------------------------------- *)
(* Result *)

(* throughout use Ast0.Neither as the third component, as its value is not
instantiated until later in the parsing process *)
let mkres e (_,lstart,_) (_,lend,_) =
  (e,{Ast0.logical_start = lstart.Ast0.logical_start;
       Ast0.logical_end = lend.Ast0.logical_end},
   ref Ast0.Neither)
    
(* --------------------------------------------------------------------- *)
    
let get_option fn = function
    None -> None
  | Some x -> Some (fn x)
	
(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Mcode *)
	
let good_mcode(_,_,mcodekind) =
  let ln =
    match mcodekind with
      Ast.MINUS(info,_) -> Ast0.Good info.Ast.logical_line
    | Ast.PLUS(info) -> Ast0.Good info.Ast.logical_line
    | Ast.CONTEXT(info,_) -> Ast0.Good info.Ast.logical_line in
  ((),{Ast0.logical_start = ln;Ast0.logical_end = ln},ref Ast0.Neither)
    
let bad_mcode(_,_,mcodekind) =
  let ln =
    match mcodekind with
      Ast.MINUS(info,_) -> Ast0.Bad info.Ast.logical_line
    | Ast.PLUS(info) -> Ast0.Bad info.Ast.logical_line
    | Ast.CONTEXT(info,_) -> Ast0.Bad info.Ast.logical_line in
  ((),{Ast0.logical_start = ln;Ast0.logical_end = ln},ref Ast0.Neither)
    
(* --------------------------------------------------------------------- *)
(* Dots *)
    
let dot_list fn = function
    [] -> failwith "dots should not be empty"
  | l -> let l = List.map fn l in (l,List.hd l,List.hd (List.rev l))
      
let dots prev fn d =
  match (prev,Ast0.unwrap d) with
    (Some prev,Ast0.DOTS([])) ->
      mkres (Ast0.DOTS []) prev prev
  | (_,Ast0.DOTS(x)) ->
      let (l,lstart,lend) = dot_list fn x in
      mkres (Ast0.DOTS l) lstart lend
  | (_,Ast0.CIRCLES(x)) ->
      let (l,lstart,lend) = dot_list fn x in
      mkres (Ast0.CIRCLES l) lstart lend
  | (_,Ast0.STARS(x)) ->
      let (l,lstart,lend) = dot_list fn x in
      mkres (Ast0.STARS l) lstart lend
	
(* --------------------------------------------------------------------- *)
(* Identifier *)
	
let rec ident i =
  match Ast0.unwrap i with
    Ast0.Id(name) -> good_mcode name
  | Ast0.MetaId(name) -> good_mcode name
  | Ast0.MetaFunc(name) -> good_mcode name
  | Ast0.MetaLocalFunc(name) -> good_mcode name
  | Ast0.OptIdent(id) -> ident id
  | Ast0.UniqueIdent(id) -> ident id
  | Ast0.MultiIdent(id) -> ident id
	
(* --------------------------------------------------------------------- *)
(* Expression *)
	
let rec expression e =
  match Ast0.unwrap e with
    Ast0.Ident(id) as e ->
      let ln = ident id in
      mkres e ln ln
  | Ast0.Constant(const) as e ->
      let ln = good_mcode const in
      mkres e ln ln
  | Ast0.FunCall(fn,lp,args,rp) ->
      let fn = expression fn in
      let args = dots (Some(good_mcode lp)) expression args in
      mkres (Ast0.FunCall(fn,lp,args,rp)) fn (good_mcode rp)
  | Ast0.Assignment(left,op,right) ->
      let left = expression left in
      let right = expression right in
      mkres (Ast0.Assignment(left,op,right)) left right
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
      let exp1 = expression exp1 in
      let exp2 = get_option expression exp2 in
      let exp3 = expression exp3 in
      mkres (Ast0.CondExpr(exp1,why,exp2,colon,exp3)) exp1 exp3
  | Ast0.Postfix(exp,op) ->
      let exp = expression exp in
      mkres (Ast0.Postfix(exp,op)) exp (good_mcode op)
  | Ast0.Infix(exp,op) ->
      let exp = expression exp in
      mkres (Ast0.Infix(exp,op)) (good_mcode op) exp
  | Ast0.Unary(exp,op) ->
      let exp = expression exp in
      mkres (Ast0.Unary(exp,op)) (good_mcode op) exp
  | Ast0.Binary(left,op,right) ->
      let left = expression left in
      let right = expression right in
      mkres (Ast0.Binary(left,op,right)) left right
  | Ast0.Paren(lp,exp,rp) ->
      mkres (Ast0.Paren(lp,expression exp,rp)) (good_mcode lp) (good_mcode rp)
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
      let exp1 = expression exp1 in
      let exp2 = expression exp2 in
      mkres (Ast0.ArrayAccess(exp1,lb,exp2,rb)) exp1 (good_mcode rb)
  | Ast0.RecordAccess(exp,pt,field) ->
      let exp = expression exp in
      mkres (Ast0.RecordAccess(exp,pt,field)) exp (ident field)
  | Ast0.RecordPtAccess(exp,ar,field) ->
      let exp = expression exp in
      mkres (Ast0.RecordPtAccess(exp,ar,field)) exp (ident field)
  | Ast0.Cast(lp,ty,rp,exp) ->
      let exp = expression exp in
      mkres (Ast0.Cast(lp,fullType ty,rp,exp)) (good_mcode lp) exp
  | Ast0.MetaConst(name,ty) as e ->
      let ln = good_mcode name in mkres e ln ln
  | Ast0.MetaErr(name) as e ->
      let ln = good_mcode name in mkres e ln ln
  | Ast0.MetaExpr(name,ty) as e ->
      let ln = good_mcode name in mkres e ln ln
  | Ast0.MetaExprList(name) as e ->
      let ln = good_mcode name in mkres e ln ln
  | Ast0.EComma(cm) as e         ->
      let ln = good_mcode cm in mkres e ln ln
  | Ast0.DisjExpr([])     -> failwith "empty dots"
  | Ast0.DisjExpr(exps)   ->
      let exps = List.map expression exps in
      mkres (Ast0.DisjExpr(List.map expression exps))
	(List.hd exps) (List.hd (List.rev exps))
  | Ast0.NestExpr(exp_dots) ->
      let exp_dots = dots None expression exp_dots in
      mkres (Ast0.NestExpr(exp_dots)) exp_dots exp_dots
  | Ast0.Edots(dots,whencode)
  | Ast0.Ecircles(dots,whencode)
  | Ast0.Estars(dots,whencode) as e ->
      let ln = bad_mcode dots in
      mkres e ln ln
  | Ast0.OptExp(exp) ->
      let exp = expression exp in
      mkres (Ast0.OptExp(exp)) exp exp
  | Ast0.UniqueExp(exp) ->
      let exp = expression exp in
      mkres (Ast0.UniqueExp(exp)) exp exp
  | Ast0.MultiExp(exp) ->
      let exp = expression exp in
      mkres (Ast0.MultiExp(exp)) exp exp
	
(* --------------------------------------------------------------------- *)
(* Types *)
	
and fullType ft =
  match Ast0.unwrap ft with
    Ast0.Type(cv,ty) ->
      let ty = typeC ty in
      (match cv with
	None -> mkres (Ast0.Type(cv,ty)) ty ty
      |	Some c -> mkres (Ast0.Type(cv,ty)) (good_mcode c) ty)
  | Ast0.OptType(ty) ->
      let ty = fullType ty in mkres (Ast0.OptType(ty)) ty ty
  | Ast0.UniqueType(ty) ->
      let ty = fullType ty in mkres (Ast0.UniqueType(ty)) ty ty
  | Ast0.MultiType(ty) ->
      let ty = fullType ty in mkres (Ast0.MultiType(ty)) ty ty
	
and typeC t =
  match Ast0.unwrap t with
    Ast0.BaseType(ty,None) as t ->
      mkres t (good_mcode ty) (good_mcode ty)
  | Ast0.BaseType(ty,Some sgn) as t ->
      mkres t (good_mcode ty) (good_mcode sgn)
  | Ast0.Pointer(ty,star) ->
      let ty = fullType ty in
      mkres (Ast0.Pointer(ty,star)) ty (good_mcode star)
  | Ast0.Array(ty,lb,size,rb) ->
      let ty = fullType ty in
      mkres (Ast0.Array(ty,lb,get_option expression size,rb))
	ty (good_mcode rb)
  | Ast0.StructUnionName(name,kind) as t ->
      mkres t (good_mcode name) (good_mcode kind)
  | Ast0.TypeName(name) as t ->
      let ln = good_mcode name in mkres t ln ln
  | Ast0.MetaType(name) as t ->
      let ln = good_mcode name in mkres t ln ln
	
(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)
	
let rec declaration d =
  match Ast0.unwrap d with
    Ast0.Init(ty,id,eq,exp,sem) ->
      let ty = fullType ty in
      let exp = expression exp in
      mkres (Ast0.Init(ty,id,eq,exp,sem)) ty (good_mcode sem)
  | Ast0.UnInit(ty,id,sem) ->
      let ty = fullType ty in
      mkres (Ast0.UnInit(ty,id,sem)) ty (good_mcode sem)
  | Ast0.OptDecl(decl) ->
      let decl = declaration decl in
      mkres (Ast0.OptDecl(declaration decl)) decl decl
  | Ast0.UniqueDecl(decl) ->
      let decl = declaration decl in
      mkres (Ast0.UniqueDecl(declaration decl)) decl decl
  | Ast0.MultiDecl(decl) ->
      let decl = declaration decl in
      mkres (Ast0.MultiDecl(declaration decl)) decl decl
	
(* --------------------------------------------------------------------- *)
(* Parameter *)
	
let rec parameterTypeDef p =
  match Ast0.unwrap p with
    Ast0.VoidParam(ty) ->
      let ty = fullType ty in mkres (Ast0.VoidParam(ty)) ty ty
  | Ast0.Param(id,ty) ->
      let ty = fullType ty in mkres (Ast0.Param(id,ty)) (ident id) ty
  | Ast0.MetaParam(name) as p -> let ln = good_mcode name in mkres p ln ln
  | Ast0.MetaParamList(name) as p -> let ln = good_mcode name in mkres p ln ln
  | Ast0.PComma(cm) as p -> let ln = good_mcode cm in mkres p ln ln
  | Ast0.Pdots(dots) as p -> let ln = bad_mcode dots in mkres p ln ln
  | Ast0.Pcircles(dots) as p -> let ln = bad_mcode dots in mkres p ln ln
  | Ast0.OptParam(param) ->
      let res = parameterTypeDef param in mkres (Ast0.OptParam(res)) res res
  | Ast0.UniqueParam(param) ->
      let res = parameterTypeDef param in mkres (Ast0.UniqueParam(res)) res res
	
let parameter_list prev = dots prev parameterTypeDef
    
(* --------------------------------------------------------------------- *)
(* Top-level code *)
    
let rec statement s =
  match Ast0.unwrap s with
    Ast0.Decl(decl) ->
      let decl = declaration decl in mkres (Ast0.Decl(decl)) decl decl
  | Ast0.Seq(lbrace,body,rbrace) -> 
      let body = dots (Some(good_mcode lbrace)) statement body in
      mkres (Ast0.Seq(lbrace,body,rbrace))
	(good_mcode lbrace) (good_mcode rbrace)
  | Ast0.ExprStatement(exp,sem) ->
      let exp = expression exp in
      mkres (Ast0.ExprStatement(exp,sem)) exp (good_mcode sem)
  | Ast0.IfThen(iff,lp,exp,rp,branch) ->
      let exp = expression exp in
      let branch = statement branch in
      mkres (Ast0.IfThen(iff,lp,exp,rp,branch)) (good_mcode iff) branch
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
      let exp = expression exp in
      let branch1 = statement branch1 in
      let branch2 = statement branch2 in
      mkres (Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2))
	(good_mcode iff) branch2
  | Ast0.While(wh,lp,exp,rp,body) ->
      let exp = expression exp in
      let body = statement body in
      mkres (Ast0.While(wh,lp,exp,rp,body)) (good_mcode wh) body
  | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
      let body = statement body in
      let exp = expression exp in
      mkres (Ast0.Do(d,body,wh,lp,exp,rp,sem)) (good_mcode d) (good_mcode sem)
  | Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body) ->
      let exp1 = get_option expression exp1 in
      let exp2 = get_option expression exp2 in
      let exp3 = get_option expression exp3 in
      let body = statement body in
      mkres (Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body))
	(good_mcode fr) body
  | Ast0.Return(ret,sem) as s -> mkres s (good_mcode ret) (good_mcode sem)
  | Ast0.ReturnExpr(ret,exp,sem) ->
      let exp = expression exp in
      mkres (Ast0.ReturnExpr(ret,exp,sem)) (good_mcode ret) (good_mcode sem)
  | Ast0.MetaStmt(name)
  | Ast0.MetaStmtList(name) as s -> let ln = good_mcode name in mkres s ln ln
  | Ast0.Exp(exp) ->
      let exp = expression exp in
      mkres (Ast0.Exp(exp)) exp exp
  | Ast0.Disj([]) -> failwith "empty dots"
  | Ast0.Disj(rule_elem_dots_list) ->
      let elems =
	List.map (function x -> dots None statement x) rule_elem_dots_list in
      mkres (Ast0.Disj(elems)) (List.hd elems) (List.hd (List.rev elems))
  | Ast0.Nest(rule_elem_dots) ->
      let rule_elem_dots = dots None statement rule_elem_dots in
      mkres (Ast0.Nest(rule_elem_dots)) rule_elem_dots rule_elem_dots
  | Ast0.Dots(dots,whencode) | Ast0.Circles(dots,whencode)
  | Ast0.Stars(dots,whencode) as s ->
      let ln = bad_mcode dots in mkres s ln ln
  | Ast0.FunDecl(None,name,lp,params,rp,lbrace,body,rbrace) ->
      let params = parameter_list (Some(good_mcode lp)) params in
      let body = dots (Some(good_mcode lbrace)) statement body in
      mkres (Ast0.FunDecl(None,name,lp,params,rp,lbrace,body,rbrace))
	(ident name) (good_mcode rbrace)
  | Ast0.FunDecl((Some s) as stg,name,lp,params,rp,lbrace,body,rbrace) ->
      let params = parameter_list (Some(good_mcode lp)) params in
      let body = dots (Some(good_mcode lbrace)) statement body in
      mkres (Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace))
	(good_mcode s) (good_mcode rbrace)
  | Ast0.OptStm(stm) ->
      let stm = statement stm in mkres (Ast0.OptStm(stm)) stm stm
  | Ast0.UniqueStm(stm) ->
      let stm = statement stm in mkres (Ast0.UniqueStm(stm)) stm stm
  | Ast0.MultiStm(stm) ->
      let stm = statement stm in mkres (Ast0.MultiStm(stm)) stm stm
	
(* --------------------------------------------------------------------- *)
(* Function declaration *)
(* Haven't thought much about arity here... *)
	
let top_level t =
  Ast0.rewrap t
    (match Ast0.unwrap t with
      Ast0.DECL(decl) -> Ast0.DECL(declaration decl)
    | Ast0.INCLUDE(inc,s) as t -> t
    | Ast0.FILEINFO(old_file,new_file) as t -> t
    | Ast0.FUNCTION(stmt) -> Ast0.FUNCTION(statement stmt)
    | Ast0.CODE(rule_elem_dots) ->
	Ast0.CODE(dots None statement rule_elem_dots)
    | Ast0.ERRORWORDS(exps) as t -> t
    | Ast0.OTHER(_) -> failwith "eliminated by top_level")
	
(* --------------------------------------------------------------------- *)
(* Entry points *)
	
let compute_lines = List.map top_level
    
