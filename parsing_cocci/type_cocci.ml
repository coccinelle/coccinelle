(* Type inference:
Just propagates information based on declarations.  Could try to infer
more precise information about expression metavariables, but not sure it is
worth it.  The most obvious goal is to distinguish between test expressions
that have pointer, integer, and boolean type when matching isomorphisms,
but perhaps other needs will become apparent. *)

type typeC = 
    ConstVol        of const_vol * typeC
  | BaseType        of baseType * sign option
  | Pointer         of typeC
  | Array           of typeC (* drop size info *)
  | StructUnionName of string * structUnion
  | TypeName        of string
  | MetaType        of string
  | Unknown (* for metavariables of type expression *^* *)

and tagged_string = string
     
and baseType = VoidType | CharType | ShortType | IntType | DoubleType
| FloatType | LongType | BoolType

and structUnion = Struct | Union

and sign = Signed | Unsigned

and const_vol = Const | Volatile

(* --------------------------------------------------------------------- *)
(* Printer *)
	
let rec typeC = function
    ConstVol(cv,ty) -> const_vol cv; typeC ty
  | BaseType(ty,None) -> baseType ty
  | BaseType(ty,Some sgn) -> sign sgn; baseType ty
  | Pointer(ty) -> typeC ty; print_string "*"
  | Array(ty) -> typeC ty; print_string "[] "
  | StructUnionName(name,kind) ->
      structUnion kind; print_string name; print_string " "
  | TypeName(name) -> print_string name; print_string " "
  | MetaType(name) -> print_string name; print_string " "
  | Unknown -> print_string "unknown "

and baseType = function
    VoidType -> print_string "void "
  | CharType -> print_string "char "
  | ShortType -> print_string "short "
  | IntType -> print_string "int "
  | DoubleType -> print_string "double "
  | FloatType -> print_string "float "
  | LongType -> print_string "long "
  | BoolType -> print_string "bool "

and structUnion = function
    Struct -> print_string "struct "
  | Union -> print_string "union "

and sign = function
    Signed -> print_string "signed "
  | Unsigned -> print_string "unsigned "

and const_vol = function
    Const -> print_string "const "
  | Volatile -> print_string "volatile "



(*
let propagate_types env =
  let optional_default = None in
  let bind x y = optional_default in (* no generic way of combining types *)

  let ident r k i =
    match Ast0.unwrap i with
      Ast0.Id(id) | Ast0.MetaId(id) ->
	(try Some(List.assoc (Ast0.unwrap_mcode_id) env)
	with Not_found -> None)
    | _ -> k i in

  let expression r k e =
    match Ast0.unwrap e with
      Ast0.Ident(id) ->
	let ty = k e in Ast0.set_type e ty; ty
    | Ast0.Constant(const) ->
    | Ast0.FunCall(fn,lp,args,rp) -> k e
    | Ast0.Assignment(left,op,right) ->
    | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
	let _ = k exp1 in
	let ty1 = k exp2 in
	let ty2 = k exp3 in
	(match (ty1,ty2) with
	  (None,None) -> None
	| (Some ty,None) -> Ast0.set_type exp3 ty; Some ty
	| (None,Some ty) -> Ast0.set_type exp2 ty; Some ty
	| (Some ty1,Some ty2) ->
	    let ty = lub_type ty1 ty2 in
	    Ast0.set_type exp2 ty; Ast0.set_type exp3 ty; Some ty)
    | Ast0.Postfix(exp,op) | Ast0.Infix(exp,op) -> (* op is dec or inc *)
	(match k exp with
	  None -> None
	| Some Ast.CharType
    | Ast0.Infix(exp,op) ->
    | Ast0.Unary(exp,op) ->
    | Ast0.Binary(left,op,right) ->
    | Ast0.Paren(lp,exp,rp) ->
    | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
    | Ast0.RecordAccess(exp,pt,field) ->
    | Ast0.RecordPtAccess(exp,ar,field) ->
    | Ast0.Cast(lp,ty,rp,exp) ->
    | Ast0.SizeOfExpr(szf,exp) ->
    | Ast0.SizeOfType(szf,lp,ty,rp) ->
    | Ast0.MetaConst(name,None) ->
    | Ast0.MetaConst(name,Some ty) ->
    | Ast0.MetaErr(name) ->
    | Ast0.MetaExpr(name,None) ->
    | Ast0.MetaExpr(name,Some ty) ->
    | Ast0.MetaExprList(name) ->
    | Ast0.EComma(cm) ->
    | Ast0.DisjExpr(_,exp_list,_) ->
    | Ast0.NestExpr(starter,expr_dots,ender) ->
    | Ast0.Edots(dots,Some whencode)
    | Ast0.Ecircles(dots,Some whencode)
    | Ast0.Estars(dots,Some whencode) ->
    | Ast0.Edots(dots,None)
    | Ast0.Ecircles(dots,None)
    | Ast0.Estars(dots,None) ->
    | Ast0.OptExp(exp) ->
    | Ast0.UniqueExp(exp) ->
    | Ast0.MultiExp(exp) ->
*)
