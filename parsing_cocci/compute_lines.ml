(* Computes starting and ending logical lines for statements and
expressions.  every node gets an index as well. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
    
(* --------------------------------------------------------------------- *)
(* Result *)

let mkres (_,_,index,mcodekind,ty) e (_,lstart,_,_,_) (_,lend,_,_,_) =
  let info =
    { Ast0.line_start = lstart.Ast0.line_start;
      Ast0.line_end = lend.Ast0.line_end;
      Ast0.logical_start = lstart.Ast0.logical_start;
      Ast0.logical_end = lend.Ast0.logical_end;
      Ast0.attachable_start = lstart.Ast0.attachable_start;
      Ast0.attachable_end = lend.Ast0.attachable_end;
      Ast0.mcode_start = lstart.Ast0.mcode_start;
      Ast0.mcode_end = lend.Ast0.mcode_end;
      Ast0.column = lstart.Ast0.column;
      Ast0.offset = lstart.Ast0.offset } in
  (e,info,index,mcodekind,ty)
    
(* --------------------------------------------------------------------- *)
    
let get_option fn = function
    None -> None
  | Some x -> Some (fn x)
	
(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Mcode *)

let promote_mcode (_,_,info,mcodekind) =
  let new_info =
    {info with
      Ast0.mcode_start = Some mcodekind; Ast0.mcode_end = Some mcodekind} in
  ((),new_info,ref (-1),ref mcodekind,None)

(* mcode is good by default *)
let bad_mcode (t,a,info,mcodekind) =
  let new_info =
    {info with Ast0.attachable_start = false; Ast0.attachable_end = false} in
  (t,a,new_info,mcodekind)

(* --------------------------------------------------------------------- *)
(* Dots *)

(* for the logline classification and the mcode field, on both sides, skip
over initial minus dots, as they don't contribute anything *)
let dot_list is_dots fn = function
    [] -> failwith "dots should not be empty"
  | l ->
      let get_node l fn =
	let first = List.hd l in
	let chosen =
	  match (is_dots first, l) with (true,_::x::_) -> x | _ -> first in
	(* get the logline decorator and the mcodekind of the chosen node *)
	fn (Ast0.get_info chosen) in
      let forward = List.map fn l in
      let backward = List.rev forward in
      let (first_attachable,first_mcode) =
	get_node forward
	  (function x -> (x.Ast0.attachable_start,x.Ast0.mcode_start)) in
      let (last_attachable,last_mcode) =
	get_node backward
	  (function x -> (x.Ast0.attachable_end,x.Ast0.mcode_end)) in
      let (first_code,first_info,first_index,first_mcodekind,first_ty) =
	List.hd forward in
      let (last_code,last_info,last_index,last_mcodekind,last_ty) =
	List.hd backward in
      let first_info =
	{ first_info with
	  Ast0.attachable_start = first_attachable;
	  Ast0.mcode_start = first_mcode } in
      let last_info =
	{ last_info with
	  Ast0.attachable_end = last_attachable;
	  Ast0.mcode_end = last_mcode } in
      let first =
	(first_code,first_info,first_index,first_mcodekind,first_ty) in
      let last = (last_code,last_info,last_index,last_mcodekind,last_ty) in
      (forward,first,last)
      
let dots is_dots prev fn d =
  match (prev,Ast0.unwrap d) with
    (Some prev,Ast0.DOTS([])) ->
      mkres d (Ast0.DOTS []) prev prev
  | (None,Ast0.DOTS([])) ->
      let (_,_,index,mcodekind,ty) = d in
      (Ast0.DOTS [],
       {(Ast0.get_info d)
       with Ast0.attachable_start = false; Ast0.attachable_end = false},
       index,mcodekind,ty)
  | (_,Ast0.DOTS(x)) ->
      let (l,lstart,lend) = dot_list is_dots fn x in
      mkres d (Ast0.DOTS l) lstart lend
  | (_,Ast0.CIRCLES(x)) ->
      let (l,lstart,lend) = dot_list is_dots fn x in
      mkres d (Ast0.CIRCLES l) lstart lend
  | (_,Ast0.STARS(x)) ->
      let (l,lstart,lend) = dot_list is_dots fn x in
      mkres d (Ast0.STARS l) lstart lend

(* --------------------------------------------------------------------- *)
(* Identifier *)
	
let rec ident i =
  match Ast0.unwrap i with
    (Ast0.Id(name)) | (Ast0.MetaId(name)) | (Ast0.MetaFunc(name))
  | (Ast0.MetaLocalFunc(name)) as ui ->
      let name = promote_mcode name in mkres i ui name name
  | Ast0.OptIdent(id) ->
      let id = ident id in mkres i (Ast0.OptIdent(id)) id id
  | Ast0.UniqueIdent(id) ->
      let id = ident id in mkres i (Ast0.UniqueIdent(id)) id id
  | Ast0.MultiIdent(id) ->
      let id = ident id in mkres i (Ast0.MultiIdent(id)) id id
	
(* --------------------------------------------------------------------- *)
(* Expression *)

let is_exp_dots e =
  match Ast0.unwrap e with
    Ast0.Edots(_,_) | Ast0.Ecircles(_,_) | Ast0.Estars(_,_) -> true
  | _ -> false

let rec expression e =
  match Ast0.unwrap e with
    Ast0.Ident(id) ->
      let id = ident id in
      mkres e (Ast0.Ident(id)) id id
  | Ast0.Constant(const) as ue ->
      let ln = promote_mcode const in
      mkres e ue ln ln
  | Ast0.FunCall(fn,lp,args,rp) ->
      let fn = expression fn in
      let args = dots is_exp_dots (Some(promote_mcode lp)) expression args in
      mkres e (Ast0.FunCall(fn,lp,args,rp)) fn (promote_mcode rp)
  | Ast0.Assignment(left,op,right) ->
      let left = expression left in
      let right = expression right in
      mkres e (Ast0.Assignment(left,op,right)) left right
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
      let exp1 = expression exp1 in
      let exp2 = get_option expression exp2 in
      let exp3 = expression exp3 in
      mkres e (Ast0.CondExpr(exp1,why,exp2,colon,exp3)) exp1 exp3
  | Ast0.Postfix(exp,op) ->
      let exp = expression exp in
      mkres e (Ast0.Postfix(exp,op)) exp (promote_mcode op)
  | Ast0.Infix(exp,op) ->
      let exp = expression exp in
      mkres e (Ast0.Infix(exp,op)) (promote_mcode op) exp
  | Ast0.Unary(exp,op) ->
      let exp = expression exp in
      mkres e (Ast0.Unary(exp,op)) (promote_mcode op) exp
  | Ast0.Binary(left,op,right) ->
      let left = expression left in
      let right = expression right in
      mkres e (Ast0.Binary(left,op,right)) left right
  | Ast0.Paren(lp,exp,rp) ->
      mkres e (Ast0.Paren(lp,expression exp,rp))
	(promote_mcode lp) (promote_mcode rp)
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
      let exp1 = expression exp1 in
      let exp2 = expression exp2 in
      mkres e (Ast0.ArrayAccess(exp1,lb,exp2,rb)) exp1 (promote_mcode rb)
  | Ast0.RecordAccess(exp,pt,field) ->
      let exp = expression exp in
      let field = ident field in
      mkres e (Ast0.RecordAccess(exp,pt,field)) exp field
  | Ast0.RecordPtAccess(exp,ar,field) ->
      let exp = expression exp in
      let field = ident field in
      mkres e (Ast0.RecordPtAccess(exp,ar,field)) exp field
  | Ast0.Cast(lp,ty,rp,exp) ->
      let exp = expression exp in
      mkres e (Ast0.Cast(lp,typeC ty,rp,exp)) (promote_mcode lp) exp
  | Ast0.SizeOfExpr(szf,exp) ->
      let exp = expression exp in
      mkres e (Ast0.SizeOfExpr(szf,exp)) (promote_mcode szf) exp
  | Ast0.SizeOfType(szf,lp,ty,rp) ->
      mkres e (Ast0.SizeOfType(szf,lp,typeC ty,rp)) 
        (promote_mcode szf)  (promote_mcode rp)
  | Ast0.MetaConst(name,_) | Ast0.MetaErr(name) | Ast0.MetaExpr(name,_)
  | Ast0.MetaExprList(name) as ue ->
      let ln = promote_mcode name in mkres e ue ln ln
  | Ast0.EComma(cm) as ue ->
      let ln = promote_mcode cm in mkres e ue ln ln
  | Ast0.DisjExpr(starter,exps,ender) ->
      let starter = bad_mcode starter in
      let ender = bad_mcode ender in
      mkres e (Ast0.DisjExpr(starter,List.map expression exps,ender))
	(promote_mcode starter) (promote_mcode ender)
  | Ast0.NestExpr(starter,exp_dots,ender) ->
      let exp_dots = dots is_exp_dots None expression exp_dots in
      let starter = bad_mcode starter in
      let ender = bad_mcode ender in
      mkres e (Ast0.NestExpr(starter,exp_dots,ender))
	(promote_mcode starter) (promote_mcode ender)
  | Ast0.Edots(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres e (Ast0.Edots(dots,whencode)) ln ln
  | Ast0.Ecircles(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres e (Ast0.Ecircles(dots,whencode)) ln ln
  | Ast0.Estars(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres e (Ast0.Estars(dots,whencode)) ln ln
  | Ast0.OptExp(exp) ->
      let exp = expression exp in
      mkres e (Ast0.OptExp(exp)) exp exp
  | Ast0.UniqueExp(exp) ->
      let exp = expression exp in
      mkres e (Ast0.UniqueExp(exp)) exp exp
  | Ast0.MultiExp(exp) ->
      let exp = expression exp in
      mkres e (Ast0.MultiExp(exp)) exp exp

and expression_dots x = dots is_exp_dots None expression x
	
(* --------------------------------------------------------------------- *)
(* Types *)
	
and typeC t =
  match Ast0.unwrap t with
    Ast0.ConstVol(cv,ty) ->
      let ty = typeC ty in
      mkres t (Ast0.ConstVol(cv,ty)) (promote_mcode cv) ty
  | Ast0.BaseType(ty,None) as ut ->
      mkres t ut (promote_mcode ty) (promote_mcode ty)
  | Ast0.BaseType(ty,Some sgn) as ut ->
      mkres t ut (promote_mcode ty) (promote_mcode sgn)
  | Ast0.Pointer(ty,star) ->
      let ty = typeC ty in
      mkres t (Ast0.Pointer(ty,star)) ty (promote_mcode star)
  | Ast0.Array(ty,lb,size,rb) ->
      let ty = typeC ty in
      mkres t (Ast0.Array(ty,lb,get_option expression size,rb))
	ty (promote_mcode rb)
  | Ast0.StructUnionName(name,kind) as ut ->
      mkres t ut (promote_mcode kind) (promote_mcode name)
  | Ast0.TypeName(name) as ut ->
      let ln = promote_mcode name in mkres t ut ln ln
  | Ast0.MetaType(name) as ut ->
      let ln = promote_mcode name in mkres t ut ln ln
  | Ast0.OptType(ty) ->
      let ty = typeC ty in mkres t (Ast0.OptType(ty)) ty ty
  | Ast0.UniqueType(ty) ->
      let ty = typeC ty in mkres t (Ast0.UniqueType(ty)) ty ty
  | Ast0.MultiType(ty) ->
      let ty = typeC ty in mkres t (Ast0.MultiType(ty)) ty ty
  | Ast0.Unknown -> failwith "only for metavariables"
	
(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)
	
let rec declaration d =
  match Ast0.unwrap d with
    Ast0.Init(ty,id,eq,exp,sem) ->
      let ty = typeC ty in
      let id = ident id in
      let exp = expression exp in
      mkres d (Ast0.Init(ty,id,eq,exp,sem)) ty (promote_mcode sem)
  | Ast0.UnInit(ty,id,sem) ->
      let ty = typeC ty in
      let id = ident id in
      mkres d (Ast0.UnInit(ty,id,sem)) ty (promote_mcode sem)
  | Ast0.DisjDecl(starter,decls,ender) ->
      let starter = bad_mcode starter in
      let ender = bad_mcode ender in
      mkres d (Ast0.DisjDecl(starter,List.map declaration decls,ender))
	(promote_mcode starter) (promote_mcode ender)
  | Ast0.OptDecl(decl) ->
      let decl = declaration decl in
      mkres d (Ast0.OptDecl(declaration decl)) decl decl
  | Ast0.UniqueDecl(decl) ->
      let decl = declaration decl in
      mkres d (Ast0.UniqueDecl(declaration decl)) decl decl
  | Ast0.MultiDecl(decl) ->
      let decl = declaration decl in
      mkres d (Ast0.MultiDecl(declaration decl)) decl decl
	
(* --------------------------------------------------------------------- *)
(* Parameter *)

let is_param_dots p =
  match Ast0.unwrap p with
    Ast0.Pdots(_) | Ast0.Pcircles(_) -> true
  | _ -> false
	
let rec parameterTypeDef p =
  match Ast0.unwrap p with
    Ast0.VoidParam(ty) ->
      let ty = typeC ty in mkres p (Ast0.VoidParam(ty)) ty ty
  | Ast0.Param(id,ty) ->
      let id = ident id in
      let ty = typeC ty in mkres p (Ast0.Param(id,ty)) ty id
  | Ast0.MetaParam(name) as up ->
      let ln = promote_mcode name in mkres p up ln ln
  | Ast0.MetaParamList(name) as up ->
      let ln = promote_mcode name in mkres p up ln ln
  | Ast0.PComma(cm) as up -> let ln = promote_mcode cm in mkres p up ln ln
  | Ast0.Pdots(dots) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres p (Ast0.Pdots(dots)) ln ln
  | Ast0.Pcircles(dots) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres p (Ast0.Pcircles(dots)) ln ln
  | Ast0.OptParam(param) ->
      let res = parameterTypeDef param in
      mkres p (Ast0.OptParam(res)) res res
  | Ast0.UniqueParam(param) ->
      let res = parameterTypeDef param in
      mkres p (Ast0.UniqueParam(res)) res res
	
let parameter_list prev = dots is_param_dots prev parameterTypeDef

(* for export *)
let parameter_dots x = dots is_param_dots None parameterTypeDef x
    
(* --------------------------------------------------------------------- *)
(* Top-level code *)

let is_stm_dots s =
  match Ast0.unwrap s with
    Ast0.Dots(_,_) | Ast0.Circles(_,_) | Ast0.Stars(_,_) -> true
  | _ -> false
    
let rec statement s =
  match Ast0.unwrap s with
    Ast0.Decl(decl) ->
      let decl = declaration decl in mkres s (Ast0.Decl(decl)) decl decl
  | Ast0.Seq(lbrace,body,rbrace) -> 
      let body =
	dots is_stm_dots (Some(promote_mcode lbrace)) statement body in
      mkres s (Ast0.Seq(lbrace,body,rbrace))
	(promote_mcode lbrace) (promote_mcode rbrace)
  | Ast0.ExprStatement(exp,sem) ->
      let exp = expression exp in
      mkres s (Ast0.ExprStatement(exp,sem)) exp (promote_mcode sem)
  | Ast0.IfThen(iff,lp,exp,rp,branch) ->
      let exp = expression exp in
      let branch = statement branch in
      mkres s (Ast0.IfThen(iff,lp,exp,rp,branch)) (promote_mcode iff) branch
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
      let exp = expression exp in
      let branch1 = statement branch1 in
      let branch2 = statement branch2 in
      mkres s (Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2))
	(promote_mcode iff) branch2
  | Ast0.While(wh,lp,exp,rp,body) ->
      let exp = expression exp in
      let body = statement body in
      mkres s (Ast0.While(wh,lp,exp,rp,body)) (promote_mcode wh) body
  | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
      let body = statement body in
      let exp = expression exp in
      mkres s (Ast0.Do(d,body,wh,lp,exp,rp,sem))
	(promote_mcode d) (promote_mcode sem)
  | Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body) ->
      let exp1 = get_option expression exp1 in
      let exp2 = get_option expression exp2 in
      let exp3 = get_option expression exp3 in
      let body = statement body in
      mkres s (Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body))
	(promote_mcode fr) body
  | Ast0.Return(ret,sem) as us ->
      mkres s us (promote_mcode ret) (promote_mcode sem)
  | Ast0.ReturnExpr(ret,exp,sem) ->
      let exp = expression exp in
      mkres s (Ast0.ReturnExpr(ret,exp,sem)) 
	(promote_mcode ret) (promote_mcode sem)
  | Ast0.MetaStmt(name)
  | Ast0.MetaStmtList(name) as us ->
      let ln = promote_mcode name in mkres s us ln ln
  | Ast0.Exp(exp) ->
      let exp = expression exp in
      mkres s (Ast0.Exp(exp)) exp exp
  | Ast0.Disj(starter,rule_elem_dots_list,ender) ->
      let starter = bad_mcode starter in
      let ender = bad_mcode ender in
      let elems =
	List.map (function x -> dots is_stm_dots None statement x)
	  rule_elem_dots_list in
      mkres s (Ast0.Disj(starter,elems,ender))
	(promote_mcode starter) (promote_mcode ender)
  | Ast0.Nest(starter,rule_elem_dots,ender) ->
      let starter = bad_mcode starter in
      let ender = bad_mcode ender in
      let rule_elem_dots = dots is_stm_dots None statement rule_elem_dots in
      mkres s (Ast0.Nest(starter,rule_elem_dots,ender))
	(promote_mcode starter) (promote_mcode ender)
  | Ast0.Dots(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres s (Ast0.Dots(dots,whencode)) ln ln
  | Ast0.Circles(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres s (Ast0.Circles(dots,whencode)) ln ln
  | Ast0.Stars(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres s (Ast0.Stars(dots,whencode)) ln ln
  | Ast0.FunDecl(None,name,lp,params,rp,lbrace,body,rbrace) ->
      let params = parameter_list (Some(promote_mcode lp)) params in
      let body =
	dots is_stm_dots (Some(promote_mcode lbrace)) statement body in
      let name = ident name in
      mkres s (Ast0.FunDecl(None,name,lp,params,rp,lbrace,body,rbrace))
	name (promote_mcode rbrace)
  | Ast0.FunDecl((Some st) as stg,name,lp,params,rp,lbrace,body,rbrace) ->
      let params = parameter_list (Some(promote_mcode lp)) params in
      let body =
	dots is_stm_dots (Some(promote_mcode lbrace)) statement body in
      mkres s (Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace))
	(promote_mcode st) (promote_mcode rbrace)
  | Ast0.OptStm(stm) ->
      let stm = statement stm in mkres s (Ast0.OptStm(stm)) stm stm
  | Ast0.UniqueStm(stm) ->
      let stm = statement stm in mkres s (Ast0.UniqueStm(stm)) stm stm
  | Ast0.MultiStm(stm) ->
      let stm = statement stm in mkres s (Ast0.MultiStm(stm)) stm stm

let statement_dots x = dots is_stm_dots None statement x
	
(* --------------------------------------------------------------------- *)
(* Function declaration *)
(* Haven't thought much about arity here... *)
	
let top_level t =
  match Ast0.unwrap t with
    Ast0.DECL(decl) ->
      let decl = declaration decl in mkres t (Ast0.DECL(decl)) decl decl
  | Ast0.INCLUDE(inc,s) -> t
  | Ast0.FILEINFO(old_file,new_file) -> t
  | Ast0.FUNCTION(stmt) ->
      let stmt = statement stmt in mkres t (Ast0.FUNCTION(stmt)) stmt stmt
  | Ast0.CODE(rule_elem_dots) ->
      let rule_elem_dots = dots is_stm_dots None statement rule_elem_dots in
      mkres t (Ast0.CODE(rule_elem_dots)) rule_elem_dots rule_elem_dots
  | Ast0.ERRORWORDS(exps) -> t
  | Ast0.OTHER(_) -> failwith "eliminated by top_level"
	
(* --------------------------------------------------------------------- *)
(* Entry points *)
	
let compute_lines = List.map top_level
    
