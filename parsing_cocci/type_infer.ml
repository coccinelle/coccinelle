module T = Type_cocci
module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0

(* Type inference:
Just propagates information based on declarations.  Could try to infer
more precise information about expression metavariables, but not sure it is
worth it.  The most obvious goal is to distinguish between test expressions
that have pointer, integer, and boolean type when matching isomorphisms,
but perhaps other needs will become apparent. *)

let rec lub_type t1 t2 =
  match (t1,t2) with
    (None,None) -> None
  | (None,Some t) -> t2
  | (Some t,None) -> t1
  | (Some t1,Some t2) ->
      let rec loop = function
	  (T.Unknown,_) -> t2
	| (_,T.Unknown) -> t1
	| (T.ConstVol(cv1,ty1),T.ConstVol(cv2,ty2)) when cv1 = cv2 ->
	    T.ConstVol(cv1,loop(ty1,ty2))
	| (T.Pointer(ty1),T.Pointer(ty2)) -> T.Pointer(loop(ty1,ty2))
	| (T.Array(ty1),T.Array(ty2)) -> T.Array(loop(ty1,ty2))
	| (_,_) -> t1 in (* arbitrarily pick the first, assume type correct *)
      Some (loop (t1,t2))

let rec propagate_types env =
  let option_default = None in
  let bind x y = option_default in (* no generic way of combining types *)

  let mcode x = option_default in

  let ident r k i =
    match Ast0.unwrap i with
      Ast0.Id(id) | Ast0.MetaId(id) ->
	(try Some(List.assoc (Ast0.unwrap_mcode id) env)
	with Not_found -> None)
    | _ -> k i in

  let expression r k e =
    let res = k e in
    let ty =
      match Ast0.unwrap e with
	Ast0.Ident(id) -> res
      | Ast0.Constant(const) ->
	  (match Ast0.unwrap_mcode const with
	    Ast.String(_) -> Some (T.Pointer(T.BaseType(T.CharType,None)))
	  | Ast.Char(_) -> Some (T.BaseType(T.CharType,None))
	  | Ast.Int(_) -> Some (T.BaseType(T.IntType,None))
	  | Ast.Float(_) ->  Some (T.BaseType(T.FloatType,None)))
      | Ast0.FunCall(fn,lp,args,rp) -> None
      | Ast0.Assignment(exp1,op,exp2) ->
	  let ty = lub_type (Ast0.get_type exp1) (Ast0.get_type exp2) in
	  Ast0.set_type exp1 ty; Ast0.set_type exp2 ty; ty
      | Ast0.CondExpr(exp1,why,Some exp2,colon,exp3) ->
	  let ty = lub_type (Ast0.get_type exp2) (Ast0.get_type exp3) in
	  Ast0.set_type exp2 ty; Ast0.set_type exp3 ty; ty
      | Ast0.CondExpr(exp1,why,None,colon,exp3) -> Ast0.get_type exp3
      | Ast0.Postfix(exp,op) | Ast0.Infix(exp,op) -> (* op is dec or inc *)
	  Ast0.get_type exp
      | Ast0.Unary(exp,op) ->
	  (match Ast0.unwrap_mcode op with
	    Ast.GetRef ->
	      (match Ast0.get_type exp with
		None -> Some (T.Pointer(T.Unknown))
	      |	Some t -> Some (T.Pointer(t)))
	  | Ast.DeRef ->
	      (match Ast0.get_type exp with
		Some (T.Pointer(t)) -> Some t
	      |	_ -> None)
	  | Ast.UnPlus -> Ast0.get_type exp
	  | Ast.UnMinus -> Ast0.get_type exp
	  | Ast.Tilde -> Ast0.get_type exp
	  | Ast.Not -> Some(T.BaseType(T.BoolType,None)))
      | Ast0.Binary(exp1,op,exp2) ->
	  let same_type = function
	      (None,None) -> Some (T.BaseType(T.IntType,None))
	    | (Some (T.Pointer ty1),Some ty2) -> Some (T.Pointer ty1)
	    | (Some ty1,Some (T.Pointer ty2)) -> Some (T.Pointer ty2)
	    | (t1,t2) ->
		let ty = lub_type (Ast0.get_type exp1) (Ast0.get_type exp2) in
		Ast0.set_type exp1 ty; Ast0.set_type exp2 ty; ty in
	  (match Ast0.unwrap_mcode op with
	    Ast.Arith(op) -> same_type (Ast0.get_type exp1, Ast0.get_type exp2)
	  | Ast.Logical(op) ->
	      let _ = same_type (Ast0.get_type exp1, Ast0.get_type exp2) in
	      Some(T.BaseType(T.BoolType,None)))
      | Ast0.Paren(lp,exp,rp) -> Ast0.get_type exp
      | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
	  (match Ast0.get_type exp1 with
	    None -> None
	  | Some (T.Array(ty)) -> Some ty
	  | Some (T.Pointer(ty)) -> Some ty
	  | Some x -> failwith "ill-typed array reference")
      | Ast0.RecordAccess(exp,pt,field) -> None
      | Ast0.RecordPtAccess(exp,ar,field) -> None
      | Ast0.Cast(lp,ty,rp,exp) -> Some(Ast0.ast0_type_to_type ty)
      | Ast0.SizeOfExpr(szf,exp) -> Some(T.BaseType(T.IntType,None))
      | Ast0.SizeOfType(szf,lp,ty,rp) -> Some(T.BaseType(T.IntType,None))
      | Ast0.MetaConst(name,Some [ty]) -> Some ty
      | Ast0.MetaConst(name,_) -> None
      | Ast0.MetaErr(name) -> None
      | Ast0.MetaExpr(name,Some [ty]) -> Some ty
      | Ast0.MetaExpr(name,ty) -> None
      | Ast0.MetaExprList(name) -> None
      | Ast0.EComma(cm) -> None
      | Ast0.DisjExpr(_,exp_list,_) ->
	  let types = List.map Ast0.get_type exp_list in
	  let combined = List.fold_left lub_type None types in
	  (match combined with
	    None -> None
	  | Some t ->
	      List.iter (function e -> Ast0.set_type e (Some t)) exp_list;
	      Some t)
      | Ast0.NestExpr(starter,expr_dots,ender) ->
	  let _ = r.V0.combiner_expression_dots expr_dots in None
      | Ast0.Edots(_,_) | Ast0.Ecircles(_,_) | Ast0.Estars(_,_) -> None
      | Ast0.OptExp(exp) -> Ast0.get_type exp
      | Ast0.UniqueExp(exp) -> Ast0.get_type exp
      | Ast0.MultiExp(exp) -> Ast0.get_type exp in
    Ast0.set_type e ty;
    ty in

  let donothing r k e = k e in

  let rec strip id =
    match Ast0.unwrap id with
      Ast0.Id(name) -> Ast0.unwrap_mcode name
    | Ast0.MetaId(name) -> Ast0.unwrap_mcode name
    | Ast0.MetaFunc(name) -> Ast0.unwrap_mcode name
    | Ast0.MetaLocalFunc(name) -> Ast0.unwrap_mcode name
    | Ast0.OptIdent(id) -> strip id
    | Ast0.UniqueIdent(id) -> strip id
    | Ast0.MultiIdent(id) -> strip id in

  (* assume that all of the declarations are at the beginning of a statement
     list, which is required by C, but not actually required by the cocci
     parser *)
  let rec process_statement_list acc = function
      [] -> ()
    | (s::ss) as all_s ->
	(match Ast0.unwrap s with
	  Ast0.Decl(decl) ->
	    let rec process_decl decl =
	      match Ast0.unwrap decl with
		Ast0.Init(ty,id,_,exp,_) ->
		  (propagate_types (acc@env)).V0.combiner_expression exp;
		  [(strip id,Ast0.ast0_type_to_type ty)]
	      | Ast0.UnInit(ty,id,_) -> [(strip id,Ast0.ast0_type_to_type ty)]
	      | Ast0.DisjDecl(_,disjs,_) ->
		  List.concat(List.map process_decl disjs)
	      | Ast0.OptDecl(decl) -> process_decl decl
	      | Ast0.UniqueDecl(decl) -> process_decl decl
	      | Ast0.MultiDecl(decl) -> process_decl decl in
	    process_statement_list ((process_decl decl)@acc) ss
	| _ ->
	    let recursor = (propagate_types acc).V0.combiner_statement in
	    List.iter (function s -> let _ = recursor s in ()) all_s) in

  let statement_dots r k d =
    match Ast0.unwrap d with
      Ast0.DOTS(l) | Ast0.CIRCLES(l) | Ast0.STARS(l) ->
	process_statement_list [] l; option_default in

  let statement r k s =
    match Ast0.unwrap s with
      Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace) ->
	let rec get_binding p =
	  match Ast0.unwrap p with
	    Ast0.Param(id,ty) -> [(strip id,Ast0.ast0_type_to_type ty)]
	  | Ast0.OptParam(param) -> get_binding param
	  | _ -> [] in
	let fenv = List.concat (List.map get_binding (Ast0.undots params)) in
	(propagate_types (fenv@env)).V0.combiner_statement_dots body
    | Ast0.IfThen(_,_,exp,_,_) | Ast0.IfThenElse(_,_,exp,_,_,_,_)
    | Ast0.While(_,_,exp,_,_) | Ast0.Do(_,_,_,_,exp,_,_) ->
	let _ = k s in
	(match Ast0.get_type exp with
	  None -> Ast0.set_type exp (Some (T.BaseType(T.IntType,None)))
	| _ -> ());
	None
    |  _ -> k s in

  V0.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing statement_dots
    ident expression donothing donothing donothing statement donothing

let type_infer code =
  let fn = (propagate_types []).V0.combiner_top_level in
  let _ = List.map fn code in
  ()
