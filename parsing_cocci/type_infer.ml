(*
* Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
* Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
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

(* "functions" that return a boolean value *)
let bool_functions = ["likely";"unlikely"]

let err wrapped ty s =
  T.typeC ty; Format.print_newline();
  failwith (Printf.sprintf "line %d: %s" (Ast0.get_line wrapped) s)

type id = Id of string | Meta of (string * string)

let rec lub_type t1 t2 =
  match (t1,t2) with
    (None,None) -> None
  | (None,Some t) -> t2
  | (Some t,None) -> t1
  | (Some t1,Some t2) ->
      let rec loop = function
	  (T.Unknown,t2) -> t2
	| (t1,T.Unknown) -> t1
	| (T.ConstVol(cv1,ty1),T.ConstVol(cv2,ty2)) when cv1 = cv2 ->
	    T.ConstVol(cv1,loop(ty1,ty2))
	| (T.Pointer(ty1),T.Pointer(ty2)) ->
	    T.Pointer(loop(ty1,ty2))
	| (ty1,T.Pointer(ty2)) -> T.Pointer(ty2)
	| (T.Pointer(ty1),ty2) -> T.Pointer(ty1)
	| (T.Array(ty1),T.Array(ty2)) -> T.Array(loop(ty1,ty2))
	| (T.TypeName(s1),t2) -> t2
	| (t1,T.TypeName(s1)) -> t1
	| (t1,_) -> t1 in (* arbitrarily pick the first, assume type correct *)
      Some (loop (t1,t2))

let lub_envs envs =
  List.fold_left
    (function acc ->
      function env ->
	List.fold_left
	  (function acc ->
	    function (var,ty) ->
	      let (relevant,irrelevant) =
		List.partition (function (x,_) -> x = var) acc in
	      match relevant with
		[] -> (var,ty)::acc
	      |	[(x,ty1)] ->
		  (match lub_type (Some ty) (Some ty1) with
		    Some new_ty -> (var,new_ty)::irrelevant
		  | None -> irrelevant)
	      |	_ -> failwith "bad type environment")
	  acc env)
    [] envs

let rec propagate_types env =
  let option_default = None in
  let bind x y = option_default in (* no generic way of combining types *)

  let mcode x = option_default in

  let ident r k i =
    match Ast0.unwrap i with
      Ast0.Id(id) ->
	(try Some(List.assoc (Id(Ast0.unwrap_mcode id)) env)
	with Not_found -> None)
    | Ast0.MetaId(id,_,_) ->
	(try Some(List.assoc (Meta(Ast0.unwrap_mcode id)) env)
	with Not_found -> None)
    | _ -> k i in

  let strip_cv = function
      Some (T.ConstVol(_,t)) -> Some t
    | t -> t in

  let expression r k e =
    let res = k e in
    let ty =
      match Ast0.unwrap e with
	Ast0.Ident(id) -> Ast0.set_type e res; res
      | Ast0.Constant(const) ->
	  (match Ast0.unwrap_mcode const with
	    Ast.String(_) -> Some (T.Pointer(T.BaseType(T.CharType,None)))
	  | Ast.Char(_) -> Some (T.BaseType(T.CharType,None))
	  | Ast.Int(_) -> Some (T.BaseType(T.IntType,None))
	  | Ast.Float(_) ->  Some (T.BaseType(T.FloatType,None)))
      | Ast0.FunCall(fn,lp,args,rp) ->
	  (match Ast0.get_type fn with
	    Some (T.FunctionPointer(ty)) -> Some ty
	  |  _ ->
	      (match Ast0.unwrap fn with
		Ast0.Ident(id) ->
		  (match Ast0.unwrap id with
		    Ast0.Id(id) ->
		      if List.mem (Ast0.unwrap_mcode id) bool_functions
		      then Some(T.BaseType(T.BoolType,None))
		      else None
		  | _ -> None)
	      |	_ -> None))
      | Ast0.Assignment(exp1,op,exp2,_) ->
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
      | Ast0.Nested(exp1,op,exp2) -> failwith "nested in type inf not possible"
      | Ast0.Binary(exp1,op,exp2) ->
	  let ty1 = Ast0.get_type exp1 in
	  let ty2 = Ast0.get_type exp2 in
	  let same_type = function
	      (None,None) -> Some (T.BaseType(T.IntType,None))
	    | (Some (T.Pointer ty1),Some ty2) ->
		Some (T.Pointer ty1)
	    | (Some ty1,Some (T.Pointer ty2)) ->
		Some (T.Pointer ty2)
	    | (t1,t2) ->
		let ty = lub_type t1 t2 in
		Ast0.set_type exp1 ty; Ast0.set_type exp2 ty; ty in
	  (match Ast0.unwrap_mcode op with
	    Ast.Arith(op) -> same_type (ty1, ty2)
	  | Ast.Logical(op) ->
	      let ty = lub_type ty1 ty2 in
	      Ast0.set_type exp1 ty; Ast0.set_type exp2 ty;
	      Some(T.BaseType(T.BoolType,None)))
      | Ast0.Paren(lp,exp,rp) -> Ast0.get_type exp
      | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
	  (match strip_cv (Ast0.get_type exp2) with
	    None -> Ast0.set_type exp2 (Some(T.BaseType(T.IntType,None)))
	  | Some(T.BaseType(T.IntType,None)) -> ()
	  | Some (T.MetaType(_,_,_)) -> ()
	  | Some (T.TypeName _) -> ()
	  | Some ty -> err exp2 ty "bad type for an array index");
	  (match strip_cv (Ast0.get_type exp1) with
	    None -> None
	  | Some (T.Array(ty)) -> Some ty
	  | Some (T.Pointer(ty)) -> Some ty
	  | Some (T.MetaType(_,_,_)) -> None
	  | Some x -> err exp1 x "ill-typed array reference")
      | Ast0.RecordAccess(exp,pt,field) ->
	  (match strip_cv (Ast0.get_type exp) with
	    None -> None
	  | Some (T.StructUnionName(_,_,_)) -> None
	  | Some (T.TypeName(_)) -> None
	  | Some (T.MetaType(_,_,_)) -> None
	  | Some x -> err exp x "non-structure type in field ref")
      | Ast0.RecordPtAccess(exp,ar,field) ->
	  (match strip_cv (Ast0.get_type exp) with
	    None -> None
	  | Some (T.Pointer(t)) ->
	      (match strip_cv (Some t) with
	      | Some (T.Unknown) -> None
	      | Some (T.MetaType(_,_,_)) -> None
	      | Some (T.TypeName(_)) -> None
	      | Some (T.StructUnionName(_,_,_)) -> None
	      | Some x ->
		  err exp (T.Pointer(t))
		    "non-structure pointer type in field ref"
	      |	_ -> failwith "not possible")
	  | Some (T.MetaType(_,_,_)) -> None
	  | Some (T.TypeName(_)) -> None
	  | Some x -> err exp x "non-structure pointer type in field ref")
      | Ast0.Cast(lp,ty,rp,exp) -> Some(Ast0.ast0_type_to_type ty)
      | Ast0.SizeOfExpr(szf,exp) -> Some(T.BaseType(T.IntType,None))
      | Ast0.SizeOfType(szf,lp,ty,rp) -> Some(T.BaseType(T.IntType,None))
      | Ast0.TypeExp(ty) -> None
      | Ast0.MetaErr(name,_,_) -> None
      | Ast0.MetaExpr(name,_,Some [ty],_,_) -> Some ty
      | Ast0.MetaExpr(name,_,ty,_,_) -> None
      | Ast0.MetaExprList(name,_,_) -> None
      | Ast0.EComma(cm) -> None
      | Ast0.DisjExpr(_,exp_list,_,_) ->
	  let types = List.map Ast0.get_type exp_list in
	  let combined = List.fold_left lub_type None types in
	  (match combined with
	    None -> None
	  | Some t ->
	      List.iter (function e -> Ast0.set_type e (Some t)) exp_list;
	      Some t)
      | Ast0.NestExpr(starter,expr_dots,ender,None,multi) ->
	  let _ = r.V0.combiner_expression_dots expr_dots in None
      | Ast0.NestExpr(starter,expr_dots,ender,Some e,multi) ->
	  let _ = r.V0.combiner_expression_dots expr_dots in
	  let _ = r.V0.combiner_expression e in None
      | Ast0.Edots(_,None) | Ast0.Ecircles(_,None) | Ast0.Estars(_,None) ->
	  None
      | Ast0.Edots(_,Some e) | Ast0.Ecircles(_,Some e)
      | Ast0.Estars(_,Some e) ->
	  let _ = r.V0.combiner_expression e in None
      | Ast0.OptExp(exp) -> Ast0.get_type exp
      | Ast0.UniqueExp(exp) -> Ast0.get_type exp in
    Ast0.set_type e ty;
    ty in

  let donothing r k e = k e in

  let rec strip id =
    match Ast0.unwrap id with
      Ast0.Id(name)              -> Id(Ast0.unwrap_mcode name)
    | Ast0.MetaId(name,_,_)        -> Meta(Ast0.unwrap_mcode name)
    | Ast0.MetaFunc(name,_,_)      -> Meta(Ast0.unwrap_mcode name)
    | Ast0.MetaLocalFunc(name,_,_) -> Meta(Ast0.unwrap_mcode name)
    | Ast0.OptIdent(id)    -> strip id
    | Ast0.UniqueIdent(id) -> strip id in

  let process_whencode notfn allfn = function
      Ast0.WhenNot(x) -> let _ = notfn x in ()
    | Ast0.WhenAlways(x) -> let _ = allfn x in ()
    | Ast0.WhenModifier(_) -> () in

  (* assume that all of the declarations are at the beginning of a statement
     list, which is required by C, but not actually required by the cocci
     parser *)
  let rec process_statement_list r acc = function
      [] -> acc
    | (s::ss) ->
	(match Ast0.unwrap s with
	  Ast0.Decl(_,decl) ->
	    let rec process_decl decl =
	      match Ast0.unwrap decl with
		Ast0.Init(_,ty,id,_,exp,_) ->
		  let _ =
		    (propagate_types acc).V0.combiner_initialiser exp in
		  [(strip id,Ast0.ast0_type_to_type ty)]
	      | Ast0.UnInit(_,ty,id,_) ->
		  [(strip id,Ast0.ast0_type_to_type ty)]
	      | Ast0.MacroDecl(_,_,_,_,_) -> []
	      | Ast0.TyDecl(ty,_) -> []
	      | Ast0.Typedef(_,_,_,_) -> []
	      | Ast0.DisjDecl(_,disjs,_,_) ->
		  List.concat(List.map process_decl disjs)
	      | Ast0.Ddots(_,_) -> [] (* not in a statement list anyway *)
	      | Ast0.OptDecl(decl) -> process_decl decl
	      | Ast0.UniqueDecl(decl) -> process_decl decl in
	    let new_acc = (process_decl decl)@acc in
	    process_statement_list r new_acc ss
	| Ast0.Dots(_,wc) ->
	    List.iter
	      (process_whencode r.V0.combiner_statement_dots
		 r.V0.combiner_statement)
	      wc;
	    process_statement_list r acc ss
	| Ast0.Disj(_,statement_dots_list,_,_) ->
	    let new_acc =
	      lub_envs
		(List.map
		   (function x -> process_statement_list r acc (Ast0.undots x))
		   statement_dots_list) in
	    process_statement_list r new_acc ss
	| _ ->
	    let _ = (propagate_types acc).V0.combiner_statement s in
	    process_statement_list r acc ss) in

  let statement_dots r k d =
    match Ast0.unwrap d with
      Ast0.DOTS(l) | Ast0.CIRCLES(l) | Ast0.STARS(l) ->
	let _ = process_statement_list r env l in option_default in
  let statement r k s =
    match Ast0.unwrap s with
      Ast0.FunDecl(_,fninfo,name,lp,params,rp,lbrace,body,rbrace) ->
	let rec get_binding p =
	  match Ast0.unwrap p with
	    Ast0.Param(ty,Some id) ->
	      [(strip id,Ast0.ast0_type_to_type ty)]
	  | Ast0.OptParam(param) -> get_binding param
	  | _ -> [] in
	let fenv = List.concat (List.map get_binding (Ast0.undots params)) in
	(propagate_types (fenv@env)).V0.combiner_statement_dots body
    | Ast0.IfThen(_,_,exp,_,_,_) | Ast0.IfThenElse(_,_,exp,_,_,_,_,_)
    | Ast0.While(_,_,exp,_,_,_) | Ast0.Do(_,_,_,_,exp,_,_)
    | Ast0.For(_,_,_,_,Some exp,_,_,_,_,_) | Ast0.Switch(_,_,exp,_,_,_,_) ->
	let _ = k s in
	let rec process_test exp =
	  match (Ast0.unwrap exp,Ast0.get_type exp) with
	    (Ast0.Edots(_,_),_) -> None
	  | (Ast0.NestExpr(_,_,_,_,_),_) -> None
	  | (Ast0.MetaExpr(_,_,_,_,_),_) ->
	    (* if a type is known, it is specified in the decl *)
	      None
	  | (Ast0.Paren(lp,exp,rp),None) -> process_test exp
	  | (_,None) -> Some (T.BaseType(T.IntType,None))
	  | _ -> None in
	let new_expty = process_test exp in
	(match new_expty with
	  None -> () (* leave things as they are *)
	| Some ty -> Ast0.set_type exp new_expty);
	None
    |  _ -> k s

  and case_line r k c =
    match Ast0.unwrap c with
      Ast0.Default(def,colon,code) -> let _ = k c in None
    | Ast0.Case(case,exp,colon,code) ->
	let _ = k c in
	(match Ast0.get_type exp with
	  None -> Ast0.set_type exp (Some (T.BaseType(T.IntType,None)))
	| _ -> ());
	None
    | Ast0.OptCase(case) -> k c in

  V0.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing statement_dots donothing donothing
    ident expression donothing donothing donothing donothing statement
    case_line donothing

let type_infer code =
  let prop = propagate_types [(Id("NULL"),T.Pointer(T.Unknown))] in
  let fn = prop.V0.combiner_top_level in
  let _ = List.map fn code in
  ()
