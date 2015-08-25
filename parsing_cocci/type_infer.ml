module T = Type_cocci
module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

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

type id = Id of string | Meta of Ast.meta_name

let int_type = T.BaseType(T.IntType)
let void_type = T.BaseType(T.VoidType)
let bool_type = T.BaseType(T.BoolType)
let char_type = T.BaseType(T.CharType)
let float_type = T.BaseType(T.FloatType)
let size_type = T.BaseType(T.SizeType)
let ssize_type = T.BaseType(T.SSizeType)
let ptrdiff_type = T.BaseType(T.PtrDiffType)

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

        (* pad: in pointer arithmetic, as in ptr+1, the lub must be ptr *)
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

  let ident r k i =
    match Ast0.unwrap i with
      Ast0.Id((s, _, _, _, _, _) as id) ->
	(try Some(List.assoc (Id(Ast0.unwrap_mcode id)) env)
	with Not_found -> None)
    | Ast0.MetaId(id,_,_,_) ->
	(try Some(List.assoc (Meta(Ast0.unwrap_mcode id)) env)
	with Not_found -> None)
    | Ast0.DisjId(_,id_list,_,_) ->
	let types = List.map Ast0.get_type id_list in
	let combined = List.fold_left lub_type None types in
	(match combined with
	  None -> None
	| Some t ->
	    List.iter (function i -> Ast0.set_type i (Some t)) id_list;
	    Some t)
    | Ast0.AsIdent _ -> failwith "not possible"
    | _ -> k i in

  let strip_cv = function
      Some (T.ConstVol(_,t)) -> Some t
    | t -> t in

  let ast0_type_to_type = Ast0.ast0_type_to_type false in

  (* types that might be integer types.  should char be allowed? *)
  let rec is_int_type = function
      T.BaseType(T.IntType)
    | T.BaseType(T.LongType)
    | T.BaseType(T.ShortType)
    | T.BaseType(T.SizeType)
    | T.MetaType(_,_,_)
    | T.TypeName _
    | T.EnumName _
    | T.SignedT(_,None) -> true
    | T.SignedT(_,Some ty) -> is_int_type ty
    | _ -> false in

  let expression r k e =
    let res = k e in
    let ty =
      match Ast0.unwrap e with
          (* pad: the type of id is set in the ident visitor *)
	  Ast0.Ident(id) -> Ast0.set_type e res; res
	| Ast0.Constant(const) ->
	    (match Ast0.unwrap_mcode const with
	         Ast.String(_) -> Some (T.Pointer(char_type))
	       | Ast.Char(_) -> Some (char_type)
	       | Ast.Int(_) -> Some (int_type)
	       | Ast.Float(_) ->  Some (float_type)
	       | Ast.DecimalConst(_,l,p) ->
		   Some (T.Decimal(T.Num l, T.Num p)))
        (* pad: note that in C can do either ptr(...) or ( *ptr)(...)
         * so I am not sure this code is enough.
         *)
	| Ast0.StringConstant _ -> Some (T.Array(char_type))
	| Ast0.FunCall(fn,lp,args,rp) ->
	    (match Ast0.get_type fn with
		 Some (T.FunctionPointer(ty)) -> Some ty
	       |  _ ->
		    (match Ast0.unwrap fn with
			 Ast0.Ident(id) ->
			   (match Ast0.unwrap id with
				Ast0.Id(id) ->
				  if List.mem (Ast0.unwrap_mcode id) bool_functions
				  then Some(bool_type)
				  else None
			      | _ -> None)
		       |	_ -> None))
	| Ast0.Assignment(exp1,op,exp2,_) ->
	    let ty = lub_type (Ast0.get_type exp1) (Ast0.get_type exp2) in
	      Ast0.set_type exp1 ty; Ast0.set_type exp2 ty; ty
	| Ast0.Sequence(exp1,op,exp2) -> Ast0.get_type exp2
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
	       | Ast.GetRefLabel -> Some (T.Pointer(void_type))
	       | Ast.DeRef ->
		   (match Ast0.get_type exp with
			Some (T.Pointer(t)) -> Some t
		      |	_ -> None)
	       | Ast.UnPlus -> Ast0.get_type exp
	       | Ast.UnMinus -> Ast0.get_type exp
	       | Ast.Tilde -> Ast0.get_type exp
	       | Ast.Not -> Some(bool_type))
	| Ast0.Nested(exp1,op,exp2) -> failwith "nested in type inf not possible"
	| Ast0.Binary(exp1,op,exp2) ->
	    let ty1 = Ast0.get_type exp1 in
	    let ty2 = Ast0.get_type exp2 in
	    let same_type = function
		(None,None) -> Some (int_type)

              (* pad: pointer arithmetic handling as in ptr+1 *)
	      | (Some (T.Pointer ty1),Some ty2) when is_int_type ty2 ->
		  Some (T.Pointer ty1)
	      | (Some ty1,Some (T.Pointer ty2)) when is_int_type ty1 ->
		  Some (T.Pointer ty2)

	      | (t1,t2) ->
		  let ty = lub_type t1 t2 in
		    Ast0.set_type exp1 ty; Ast0.set_type exp2 ty; ty in
	      (match Ast0.unwrap op with
		 Ast0.Arith(op) -> same_type (ty1, ty2)
                 | Ast0.MetaBinary _ -> same_type (ty1, ty2)
		 | Ast0.Logical(op') when (let op''=Ast0.unwrap_mcode op' in op''=Ast.AndLog || op''=Ast.OrLog) ->
		     Some(bool_type)
		 | Ast0.Logical(op) ->
		     let ty = lub_type ty1 ty2 in
		     Ast0.set_type exp1 ty; Ast0.set_type exp2 ty;
		     Some(bool_type))
	| Ast0.Paren(lp,exp,rp) -> Ast0.get_type exp
	| Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
	    (match strip_cv (Ast0.get_type exp2) with
		 None -> Ast0.set_type exp2 (Some(int_type))
	       | Some(ty) when is_int_type ty -> ()
	       | Some(Type_cocci.Unknown) ->
		   (* unknown comes from param types, not sure why this
		      is not just None... *)
		   Ast0.set_type exp2 (Some(int_type))
	       | Some ty -> err exp2 ty "bad type for an array index");
	    (match strip_cv (Ast0.get_type exp1) with
		 None -> None
	       | Some (T.Array(ty)) -> Some ty
	       | Some (T.Pointer(ty)) -> Some ty
	       | Some (T.MetaType(_,_,_)) -> None
	       | Some x -> err exp1 x "ill-typed array reference")
	      (* pad: should handle structure one day and look 'field' in environment *)
	| Ast0.RecordAccess(exp,pt,field) ->
	    (match strip_cv (Ast0.get_type exp) with
		 None -> None
	       | Some (T.StructUnionName(_,_)) -> None
	       | Some (T.TypeName(s)) ->
			  None
	       | Some (T.MetaType(_,_,_)) -> None
	       | Some x -> err exp x "non-structure type in field ref")
	| Ast0.RecordPtAccess(exp,ar,field) ->
	    (match strip_cv (Ast0.get_type exp) with
		 None -> None
	       | Some (T.Pointer(t)) ->
		   (match strip_cv (Some t) with
		      | Some (T.Unknown) -> None
		      | Some (T.MetaType(_,_,_)) -> None
		      | Some (T.TypeName(s)) ->
			  None
		      | Some (T.StructUnionName(s,t)) ->
			  None
		      | Some x ->
			  err exp (T.Pointer(t))
			    "non-structure pointer type in field ref"
		      |	_ -> failwith "not possible")
	       | Some (T.MetaType(_,_,_)) -> None
	       | Some (T.TypeName(s)) ->
		   None
	       | Some x -> err exp x "non-structure pointer type in field ref")
	| Ast0.Cast(lp,ty,rp,exp) -> Some(ast0_type_to_type ty)
	| Ast0.SizeOfExpr(szf,exp) -> Some(int_type)
	| Ast0.SizeOfType(szf,lp,ty,rp) -> Some(int_type)
	| Ast0.TypeExp(ty) -> None
	| Ast0.Constructor(lp,ty,rp,init) -> Some(ast0_type_to_type ty)
	| Ast0.MetaErr(name,_,_) -> None
	| Ast0.MetaExpr(name,_,Some [ty],_,_) -> Some ty
	| Ast0.MetaExpr(name,_,ty,_,_) -> None
	| Ast0.MetaExprList(name,_,_) -> None
	| Ast0.EComma(cm) -> None
	| Ast0.DisjExpr(_,exp_list,_,_)
	| Ast0.ConjExpr(_,exp_list,_,_) ->
	    let types = List.map Ast0.get_type exp_list in
	    let combined = List.fold_left lub_type None types in
	      (match combined with
		   None -> None
		 | Some t ->
		     List.iter (function e -> Ast0.set_type e (Some t)) exp_list;
		     Some t)
	| Ast0.NestExpr(starter,expr_dots,ender,None,multi) ->
	    let _ = r.VT0.combiner_rec_expression_dots expr_dots in None
	| Ast0.NestExpr(starter,expr_dots,ender,Some (_,_,e),multi) ->
	    let _ = r.VT0.combiner_rec_expression_dots expr_dots in
	    let _ = r.VT0.combiner_rec_expression e in None
	| Ast0.Edots(_,None) -> None
	| Ast0.Edots(_,Some (_,_,e)) ->
	    let _ = r.VT0.combiner_rec_expression e in None
	| Ast0.OptExp(exp) -> Ast0.get_type exp
	| Ast0.AsExpr _ | Ast0.AsSExpr _ -> failwith "not possible" in
      Ast0.set_type e ty;
      ty in

  let rec strip id =
    match Ast0.unwrap id with
      Ast0.Id(name)                -> [Id(Ast0.unwrap_mcode name)]
    | Ast0.MetaId(name,_,_,_)      -> [Meta(Ast0.unwrap_mcode name)]
    | Ast0.MetaFunc(name,_,_)      -> [Meta(Ast0.unwrap_mcode name)]
    | Ast0.MetaLocalFunc(name,_,_) -> [Meta(Ast0.unwrap_mcode name)]
    | Ast0.DisjId(_,id_list,_,_)   -> List.concat (List.map strip id_list)
    | Ast0.OptIdent(id)            -> strip id
    | Ast0.AsIdent _ -> failwith "not possible" in

  let process_whencode notfn allfn exp = function
      Ast0.WhenNot(_,_,x) -> let _ = notfn x in ()
    | Ast0.WhenAlways(_,_,x) -> let _ = allfn x in ()
    | Ast0.WhenModifier _ -> ()
    | Ast0.WhenNotTrue(_,_,x) -> let _ = exp x in ()
    | Ast0.WhenNotFalse(_,_,x) -> let _ = exp x in () in

  (* assume that all of the declarations are at the beginning of a statement
     list, which is required by C, but not actually required by the cocci
     parser *)
  let rec process_statement_list r acc = function
      [] -> acc
    | (s::ss) ->
	(match Ast0.unwrap s with
	  Ast0.Decl(_,decl) ->
	    let new_acc = (process_decl acc decl)@acc in
	    process_statement_list r new_acc ss
	| Ast0.Dots(_,wc) ->
	    (* why is this case here?  why is there none for nests? *)
	    List.iter
	      (process_whencode r.VT0.combiner_rec_statement_dots
		 r.VT0.combiner_rec_statement r.VT0.combiner_rec_expression)
	      wc;
	    process_statement_list r acc ss
	| Ast0.Disj(_,statement_dots_list,_,_)
	| Ast0.Conj(_,statement_dots_list,_,_) ->
	    let new_acc =
	      lub_envs
		(List.map
		   (function x -> process_statement_list r acc (Ast0.unwrap x))
		   statement_dots_list) in
	    process_statement_list r new_acc ss
	| _ ->
	    let _ = (propagate_types acc).VT0.combiner_rec_statement s in
	    process_statement_list r acc ss)

  and process_decl env decl =
    match Ast0.unwrap decl with
      Ast0.MetaDecl(_,_) | Ast0.MetaField(_,_)
    | Ast0.MetaFieldList(_,_,_) -> []
    | Ast0.Init(_,ty,id,_,exp,_) ->
	let _ = (propagate_types env).VT0.combiner_rec_initialiser exp in
	let ty = ast0_type_to_type ty in
	List.map (function i -> (i,ty)) (strip id)
    | Ast0.UnInit(_,ty,id,_) ->
	let ty = ast0_type_to_type ty in
	List.map (function i -> (i,ty)) (strip id)
    | Ast0.FunProto(fi,nm,lp,params,va,rp,sem) -> []
    | Ast0.MacroDecl(_,_,_,_,_,_) -> []
    | Ast0.MacroDeclInit(_,_,_,_,_,_,exp,_) ->
        let _ = (propagate_types env).VT0.combiner_rec_initialiser exp in
	[]
    | Ast0.TyDecl(ty,_) -> []
              (* pad: should handle typedef one day and add a binding *)
    | Ast0.Typedef((a,_,_,_,_,_),b,c,(d,_,_,_,_,_)) ->
	[]
    | Ast0.DisjDecl(_,disjs,_,_) ->
	List.concat(List.map (process_decl env) disjs)
    | Ast0.Ddots(_,_) -> [] (* not in a statement list anyway *)
    | Ast0.OptDecl(decl) -> process_decl env decl
    | Ast0.AsDecl _ -> failwith "not possible" in

  let statement_dots r k d =
    let _ = process_statement_list r env (Ast0.unwrap d) in option_default in

  let post_bool exp =
    let rec process_test exp =
      match (Ast0.unwrap exp,Ast0.get_type exp) with
	(Ast0.Edots(_,_),_) -> None
      | (Ast0.NestExpr(_,_,_,_,_),_) -> None
      | (Ast0.MetaExpr(_,_,_,_,_),_) ->
      (* if a type is known, it is specified in the decl *)
	  None
      | (Ast0.Paren(lp,exp,rp),None) -> process_test exp
      (* the following doesn't seem like a good idea - triggers int isos
	 on all test expressions *)
      (*| (_,None) -> Some (int_type) *)
      | _ -> None in
    let new_expty = process_test exp in
    (match new_expty with
      None -> () (* leave things as they are *)
    | Some ty -> Ast0.set_type exp new_expty) in

  let statement r k s =
    match Ast0.unwrap s with
      Ast0.FunDecl(_,fninfo,name,lp,params,va,rp,lbrace,body,rbrace,_) ->
	let rec get_binding p =
	  match Ast0.unwrap p with
	    Ast0.Param(ty,Some id) ->
	      let ty = ast0_type_to_type ty in
	      List.map (function i -> (i,ty)) (strip id)
	  | Ast0.OptParam(param) -> get_binding param
	  | Ast0.AsParam(param,e) -> get_binding param
	  | _ -> [] in
	let fenv = List.concat (List.map get_binding (Ast0.unwrap params)) in
	(propagate_types (fenv@env)).VT0.combiner_rec_statement_dots body
    | Ast0.IfThen(_,_,exp,_,_,_) | Ast0.IfThenElse(_,_,exp,_,_,_,_,_)
    | Ast0.While(_,_,exp,_,_,_) | Ast0.Do(_,_,_,_,exp,_,_) ->
	let _ = k s in
        post_bool exp;
        None
    | Ast0.For(a,b,first,exp,c,d,e,f,g) ->
	(match Ast0.unwrap first with
	  Ast0.ForExp _ ->
	    (match exp with
	      Some exp ->
		let _ = k s in
		post_bool exp;
		None
	    | None -> k s)
	| Ast0.ForDecl (_,decl) ->
	(* not super elegant..., reuses a ; (d) *)
	    let newenv = (process_decl env decl)@env in
	    let dummy = Ast0.rewrap first (Ast0.ForExp (None,c)) in
	    (propagate_types newenv).VT0.combiner_rec_statement
	      (Ast0.rewrap s (Ast0.For(a,b,dummy,exp,c,d,e,f,g))))
    | Ast0.Switch(_,_,exp,_,_,decls,cases,_) ->
	let senv = process_statement_list r env (Ast0.unwrap decls) in
	let res =
	  (propagate_types (senv@env)).VT0.combiner_rec_case_line_dots cases in
	post_bool exp;
	res
    |  _ -> k s

  and case_line r k c =
    match Ast0.unwrap c with
      Ast0.Case(case,exp,colon,code) ->
	let _ = k c in
	(match Ast0.get_type exp with
	  None -> Ast0.set_type exp (Some (int_type))
	| _ -> ());
	None
    | _ -> k c in

  V0.combiner bind option_default
    {V0.combiner_functions with
      VT0.combiner_dotsstmtfn = statement_dots;
      VT0.combiner_identfn = ident;
      VT0.combiner_exprfn = expression;
      VT0.combiner_stmtfn = statement;
      VT0.combiner_casefn = case_line}

let type_infer code =
  let prop = propagate_types [(Id("NULL"),T.Pointer(T.Unknown))] in
  let fn = prop.VT0.combiner_rec_top_level in
  let _ = List.map fn code in
  ()
