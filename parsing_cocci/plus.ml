(* The plus fragments are converted to a list of lists of lists.
Innermost list: Elements have type anything.  For any pair of successive
elements, n and n+1, the ending line of n is the same as the starting line
of n+1.
Middle lists: For any pair of successive elements, n and n+1, the ending
line of n is one less than the starting line of n+1.
Outer list: For any pair of successive elements, n and n+1, the ending
line of n is more than one less than the starting line of n+1. *)

(* For nests and disjs, we are relying on the fact that <... ...> ( | )
must appear on lines by themselves, meaning that the various + fragments
can't be contiguous to each other or to unrelated things. *)

module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)

type res =
    Open of Ast.anything * int * int * int * int
  | Closed of (Ast.anything * int * int * int * int) list

let mcode fn = function
    term, Ast.PLUS(info) ->
      let line = info.Ast.line in
      let lline = info.Ast.logical_line in
      Open (fn term,line,line,lline,lline)
  | _ -> Closed []

let mk_fullType x         = Ast.FullTypeTag x
let mk_baseType x         = Ast.BaseTypeTag x
let mk_structUnion x      = Ast.StructUnionTag x
let mk_sign x             = Ast.SignTag x
let mk_ident x            = Ast.IdentTag x
let mk_expression x       = Ast.ExpressionTag x
let mk_constant x         = Ast.ConstantTag x
let mk_unaryOp x          = Ast.UnaryOpTag x
let mk_assignOp x         = Ast.AssignOpTag x
let mk_fixOp x            = Ast.FixOpTag x
let mk_binaryOp x         = Ast.BinaryOpTag x
let mk_arithOp x          = Ast.ArithOpTag x
let mk_logicalOp x        = Ast.LogicalOpTag x
let mk_declaration x      = Ast.DeclarationTag x
let mk_storage x          = Ast.StorageTag x
let mk_rule_elem x        = Ast.Rule_elemTag x
let mk_value_qualif x     = Ast.ValueQualifTag x
let mk_token x            = Ast.Token x

let get_real_start = function
    Open (_,line,_,_,_) -> line
  | _ -> failwith "not possible"

let get_real_finish = function
    Open (_,_,line,_,_) -> line
  | _ -> failwith "not possible"

let get_start = function
    Open (_,_,_,line,_) -> line
  | _ -> failwith "not possible"

let get_finish = function
    Open (_,_,_,_,line) -> line
  | _ -> failwith "not possible"

let get_option fn = function
    None -> []
  | Some x -> [fn x]

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Step 1: coalesce + terms, record starting and ending line numbers *)

let rec close l =
  let rec loop = function
      [] -> []
    | Open(x,start,finish,lstart,lfinish)::rest ->
	(x,start,finish,lstart,lfinish)::(loop rest)
    | (Closed l)::rest -> l @ (loop rest) in
  Closed (loop l)

let test term subterms =
  if List.for_all (function Open(_,_,_,_,_) -> true | _ -> false) subterms
  then Open(term,
	    get_real_start (List.hd subterms),
	    get_real_finish (List.hd (List.rev subterms)),
	    get_start (List.hd subterms),
	    get_finish (List.hd (List.rev subterms)))
  else close subterms

(* --------------------------------------------------------------------- *)
(* Dots *)

let dots fn dotlist =
  let inner_elems =
    match dotlist with
      Ast.DOTS(x) -> x
    | Ast.CIRCLES(x) -> x
    | Ast.STARS(x) -> x in
  close (List.map fn inner_elems)

(* --------------------------------------------------------------------- *)
(* Identifier *)

let ident x =
  let subterms =
    match x with
      Ast.Id(name) -> [mcode mk_token name]
    | Ast.MetaId(name) -> [mcode mk_token name]
    | Ast.MetaFunc(name) -> [mcode mk_token name]
    | Ast.MetaLocalFunc(name) -> [mcode mk_token name]
    | Ast.OptIdent(_) | Ast.UniqueIdent(_) | Ast.MultiIdent(_) ->
	failwith "impossible" in
  test (Ast.IdentTag x) subterms

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression x =
  let subterms =
    match x with
      Ast.Ident(id) -> [ident id]
    | Ast.Constant(const) -> [mcode mk_constant const]
    | Ast.FunCall(fn,lp,args,rp) ->
	[expression fn; mcode mk_token lp; dots expression args;
	  mcode mk_token rp]
    | Ast.Assignment(left,op,right) ->
	[expression left; mcode mk_assignOp op; expression right]
    | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
	[expression exp1; mcode mk_token why] @
	(match exp2 with Some e -> [expression e] | _ -> []) @
	[mcode mk_token colon; expression exp3]
    | Ast.Postfix(exp,op) ->
	[expression exp; mcode mk_fixOp op]
    | Ast.Infix(exp,op) ->
	[mcode mk_fixOp op; expression exp]
    | Ast.Unary(exp,op) ->
	[mcode mk_unaryOp op; expression exp]
    | Ast.Binary(left,op,right) ->
	[expression left; mcode mk_binaryOp op; expression right]
    | Ast.Paren(lp,exp,rp) ->
	[mcode mk_token lp; expression exp; mcode mk_token rp]
    | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
	[expression exp1; mcode mk_token lb; expression exp2;
	  mcode mk_token rb]
    | Ast.RecordAccess(exp,pt,field) ->
	[expression exp; mcode mk_token pt; ident field]
    | Ast.RecordPtAccess(exp,ar,field) ->
	[expression exp; mcode mk_token ar; ident field]
    | Ast.Cast(lp,ty,rp,exp) ->
	[mcode mk_token lp; typeC ty; mcode mk_token rp; expression exp]
    | Ast.MetaConst(name,ty)  -> [mcode mk_token name]
    | Ast.MetaErr(name)  -> [mcode mk_token name]
    | Ast.MetaExpr(name,ty)  -> [mcode mk_token name]
    | Ast.MetaExprList(name) -> [mcode mk_token name]
    | Ast.EComma(cm)         -> [mcode mk_token cm]
    | Ast.DisjExpr(exps)     ->
	List.map (function x -> close [expression x]) exps
    | Ast.NestExpr(exp_dots) -> [dots expression exp_dots]
    | Ast.Edots(_,_)    -> [Closed []] (* must be context *)
    | Ast.Ecircles(_,_) -> [Closed []] (* must be context *)
    | Ast.Estars(_,_)   -> [Closed []] (* must be context *)
    | Ast.OptExp(_) | Ast.UniqueExp(_) | Ast.MultiExp(_) ->
	failwith "impossible" in
  test (Ast.ExpressionTag x) subterms

(* --------------------------------------------------------------------- *)
(* Types *)

and typeC x =
  let subterms =
    match x with
      Ast.BaseType(ty,Some sign) -> [mcode mk_sign sign; mcode mk_baseType ty]
    | Ast.BaseType(ty,None) -> [mcode mk_baseType ty]
    | Ast.Pointer(ty,star)  -> [typeC ty; mcode mk_token star]
    | Ast.Array(ty,lb,size,rb)  ->
	[typeC ty; mcode mk_token lb] @ (get_option expression size) @
	[mcode mk_token rb]
    | Ast.StructUnionName(name,kind) ->
	[mcode mk_structUnion kind; mcode mk_token name]
    | Ast.TypeName(name)         -> [mcode mk_token name]
    | Ast.MetaType(name)         -> [mcode mk_token name]
    | Ast.OptType(_) | Ast.UniqueType(_) | Ast.MultiType(_) ->
	failwith "impossible" in
  test (Ast.FullTypeTag x) subterms

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

let declaration x =
  let subterms =
    match x with
      Ast.Init(ty,id,eq,exp,sem) ->
	[typeC ty; ident id; mcode mk_token eq; expression exp;
	  mcode mk_token sem]
    | Ast.UnInit(ty,id,sem) -> [typeC ty; ident id; mcode mk_token sem]
    | Ast.OptDecl(_) | Ast.UniqueDecl(_) | Ast.MultiDecl(_) ->
	failwith "impossible" in
  test (Ast.DeclarationTag x) subterms

(* --------------------------------------------------------------------- *)
(* Parameter *)

let parameterTypeDef x =
  let subterms =
    match x with
      Ast.VoidParam(ty)        -> [typeC ty]
    | Ast.Param(id,None,ty)    -> [typeC ty; ident id]
    | Ast.Param(id,Some vs,ty) ->
	[mcode mk_value_qualif vs; typeC ty; ident id]
    | Ast.MetaParam(name)      -> [mcode mk_token name]
    | Ast.MetaParamList(name)  -> [mcode mk_token name]
    | Ast.PComma(cm)           -> [mcode mk_token cm]
    | Ast.Pdots(_)             -> [Closed []]
    | Ast.Pcircles(_)          -> [Closed []]
    | Ast.OptParam(_) | Ast.UniqueParam(_) -> failwith "impossible" in
  test (Ast.ParameterTypeDefTag x) subterms

let parameter_list = dots parameterTypeDef

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rec rule_elem x =
  let subterms =
    match x with
      Ast.FunDecl(stor,nm,lp,plist,rp) ->
	(match stor with Some x -> [mcode mk_storage x] | _ -> []) @
	[ident nm; mcode mk_token lp; parameter_list plist;
	  mcode mk_token rp]
    | Ast.Decl(decl) -> [declaration decl]
    | Ast.SeqStart(brace) -> [mcode mk_token brace]
    | Ast.SeqEnd(brace) -> [mcode mk_token brace]
    | Ast.ExprStatement(exp,sem) -> [expression exp; mcode mk_token sem]
    | Ast.IfHeader(iff,lp,exp,rp) ->
	[mcode mk_token iff; mcode mk_token lp; expression exp;
	  mcode mk_token rp]
    | Ast.Else(els) -> [mcode mk_token els]
    | Ast.WhileHeader(wh,lp,exp,rp) ->
	[mcode mk_token wh; mcode mk_token lp; expression exp;
	  mcode mk_token rp]
    | Ast.Do(d) -> [mcode mk_token d]
    | Ast.WhileTail(wh,lp,exp,rp,sem) ->
	[mcode mk_token wh; mcode mk_token lp; expression exp;
	  mcode mk_token rp; mcode mk_token sem]
    | Ast.ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp) ->
	[mcode mk_token fr; mcode mk_token lp] @
	(get_option expression e1) @ [mcode mk_token sem1] @
	(get_option expression e2) @ [mcode mk_token sem2] @
	(get_option expression e3) @ [mcode mk_token rp]
    | Ast.Return(ret,sem) -> [mcode mk_token ret; mcode mk_token sem]
    | Ast.ReturnExpr(ret,exp,sem) ->
	[mcode mk_token ret; expression exp; mcode mk_token sem]
    | Ast.MetaStmt(name) -> [mcode mk_token name]
    | Ast.MetaStmtList(name) -> [mcode mk_token name]
    | Ast.Disj(rule_elem_dots_list) ->
	List.map (function x -> close [dots rule_elem x]) rule_elem_dots_list
    | Ast.Nest(rule_elem_dots) -> [dots rule_elem rule_elem_dots]
    | Ast.Exp(exp)   -> [expression exp]
    | Ast.Dots(_,_)    -> [Closed []]
    | Ast.Circles(_,_) -> [Closed []]
    | Ast.Stars(_,_)   -> [Closed []]
    | Ast.OptRuleElem(_) | Ast.UniqueRuleElem(_) | Ast.MultiRuleElem(_) ->
	failwith "impossible" in
  test (Ast.Rule_elemTag x) subterms

let top_level x =
  let subterms =
    match x with
      Ast.DECL(decl) -> [declaration decl]
    | Ast.INCLUDE(inc,name) -> [mcode mk_token inc; mcode mk_token name]
    | Ast.FILEINFO(_,_) -> [Closed []]
    | Ast.FUNCTION(rule_elem_dots) -> [dots rule_elem rule_elem_dots]
    | Ast.CODE(rule_elem_dots) -> [dots rule_elem rule_elem_dots]
    | Ast.ERRORWORDS(exps) -> [Closed []] in
  test (Ast.Code x) subterms

let rule code = List.map top_level code

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Step 2: find neighbors *)

let rec find_neighbors = function
    [] -> []
  | (x1,real_start1,real_finish1,start1,finish1)::rest ->
      (match find_neighbors rest with
	((((x2,real_start2,real_finish2,start2,finish2)::
	   rest_inner)::rest_middle)::rest_outer)
	  as rest ->
	    if finish1 = start2
	    then 
	      ((((x1,real_start1,real_finish1,start1,finish1)::
		 (x2,real_start2,real_finish2,start2,finish2)::rest_inner)::
		rest_middle)::
	       rest_outer)
	    else if finish1 + 1 = start2
	    then
	      (([(x1,real_start1,real_finish1,start1,finish1)]::
		((x2,real_start2,real_finish2,start2,finish2)::rest_inner)::
		rest_middle)::
	       rest_outer)
	    else [[(x1,real_start1,real_finish1,start1,finish1)]]::rest
      |	_ -> [[[(x1,real_start1,real_finish1,start1,finish1)]]])
      (* rest must be [] *)
      
(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Entry point *)

let plus ast =
  match close (rule ast) with
    Closed l -> find_neighbors l
  | _ -> failwith "impossible"
