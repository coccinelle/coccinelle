(* Detects subtrees that are all negative and nodes that are "binding
context nodes".  The latter is a node whose structure and immediate tokens
are the same in the minus and plus trees, and such that for every child,
the set of context nodes in the child subtree is the same in the minus and
plus subtrees. *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0

(* --------------------------------------------------------------------- *)
(* Generic access to code *)

type anything =
    DotsExpr of Ast0.expression Ast0.dots
  | DotsParam of Ast0.parameterTypeDef Ast0.dots
  | DotsStmt of Ast0.statement Ast0.dots
  | Ident of Ast0.ident
  | Expr of Ast0.expression
  | FullType of Ast0.fullType
  | TypeC of Ast0.typeC
  | Param of Ast0.parameterTypeDef
  | Decl of Ast0.declaration
  | Stmt of Ast0.statement
  | Top of Ast0.top_level

let mkDotsExpr e = DotsExpr e
let mkDotsParam e = DotsParam e
let mkDotsStmt e = DotsStmt e
let mkIdent e = Ident e
let mkExpr e = Expr e
let mkFullType e = FullType e
let mkTypeC e = TypeC e
let mkParam e = Param e
let mkDecl e = Decl e
let mkStmt e = Stmt e
let mkTop e = Top e

let set_tinfo x tinfo =
  match x with
    DotsExpr(d) -> Ast0.set_tinfo d tinfo
  | DotsParam(d) -> Ast0.set_tinfo d tinfo
  | DotsStmt(d) -> Ast0.set_tinfo d tinfo
  | Ident(d) -> Ast0.set_tinfo d tinfo
  | Expr(d) -> Ast0.set_tinfo d tinfo
  | FullType(d) -> Ast0.set_tinfo d tinfo
  | TypeC(d) -> Ast0.set_tinfo d tinfo
  | Param(d) -> Ast0.set_tinfo d tinfo
  | Decl(d) -> Ast0.set_tinfo d tinfo
  | Stmt(d) -> Ast0.set_tinfo d tinfo
  | Top(d) -> Ast0.set_tinfo d tinfo

(* --------------------------------------------------------------------- *)

type kind = AllMinus | NotAllMinus

(* --------------------------------------------------------------------- *)
(* The first part analyzes each of the minus tree and the plus tree
separately *)

(* ints are unique token indices (offset field) *)
type node =
    Token (*tokens*) of kind * Ast.mcodekind (* contains offset, so unique *)
	* int list (* context token descendents *)
  | Recursor (*children*) of kind * int list
  | Bind (*neighbors*) of kind * Ast.mcodekind list (*tokens at current level*)
	* int list list

(* goal: detect negative in both tokens and recursors, or context only in
tokens *)
let bind c1 c2 =
  let lub k1 k2 = if k1 = k2 then k1 else NotAllMinus in
  match (c1,c2) with
    (* token/token *)
    (Token(k1,t1,l1),Token(k2,t2,l2)) -> Bind(lub k1 k2,[t1;t2],[l1;l2])

    (* token/recursor *)
  | (Token(k1,t1,l1),Recursor(k2,l2)) -> Bind(lub k1 k2,[t1],[l1;l2])
  | (Recursor(k1,l1),Token(k2,t2,l2)) -> Bind(lub k1 k2,[t2],[l1;l2])

    (* token/bind *)
  | (Token(k1,t1,l1),Bind(k2,t2,l2)) -> Bind(lub k1 k2,t1::t2,l1::l2)
  | (Bind(k1,t1,l1),Token(k2,t2,l2)) -> Bind(lub k1 k2,t1@[t2],l1@[l2])

    (* recursor/bind *)
  | (Recursor(k1,l1),Bind(k2,t2,l2)) -> Bind(lub k1 k2,t2,l1::l2)
  | (Bind(k1,t1,l1),Recursor(k2,l2)) -> Bind(lub k1 k2,t1,l1@[l2])

    (* recursor/recursor and bind/bind - not likely to ever occur *)
  | (Recursor(k1,l1),Recursor(k2,l2)) -> Bind(lub k1 k2,[],[l1;l2])
  | (Bind(k1,t1,l1),Bind(k2,t2,l2)) -> Bind(lub k1 k2,t1@t2,l1@l2)


let option_default = Bind(AllMinus,[],[])

let mcode (_,_,mcodekind) =
  match mcodekind with
    Ast.MINUS(info,_) -> Token(AllMinus,mcodekind,[info.Ast.offset])
  | Ast.PLUS(info) -> Token(NotAllMinus,mcodekind,[info.Ast.offset])
  | Ast.CONTEXT(info,_) -> Token(NotAllMinus,mcodekind,[info.Ast.offset])

let is_context = function Ast.CONTEXT(_,_) -> true | _ -> false

let union_all l = List.fold_left Common.union_set [] l

let classify table code =
  let compute_result builder e = function
      Bind(k,tl,l) ->
	if k = AllMinus
	then Ast0.set_tinfo e Ast0.AllMinus (* definitive *)
	else
	  if List.for_all is_context tl
	  then Hashtbl.add table tl (builder e,union_all l);
	Recursor(k, List.fold_left (@) [] l)
    | _ -> failwith "not possible as the result of a recursive call" in
	  
  let do_nothing builder r k e = compute_result builder e (k e) in

  (* no whencode in plus tree so have to drop it *)
  let expression r k e =
    compute_result mkExpr e
      (match Ast0.unwrap e with
	Ast0.Edots(dots,whencode) ->
	  k (Ast0.rewrap e (Ast0.Edots(dots,None)))
      | Ast0.Ecircles(dots,whencode) ->
	  k (Ast0.rewrap e (Ast0.Ecircles(dots,None)))
      | Ast0.Estars(dots,whencode) ->
	  k (Ast0.rewrap e (Ast0.Estars(dots,None)))
      |	_ -> k e) in

  let statement r k s =
    compute_result mkStmt s
      (match Ast0.unwrap s with
	Ast0.Dots(dots,whencode) ->
	  k (Ast0.rewrap s (Ast0.Dots(dots,None)))
      | Ast0.Circles(dots,whencode) ->
	  k (Ast0.rewrap s (Ast0.Circles(dots,None)))
      | Ast0.Stars(dots,whencode) ->
	  k (Ast0.rewrap s (Ast0.Stars(dots,None)))
      |	_ -> k s) in

  let combiner = 
    V0.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      (do_nothing mkDotsExpr) (do_nothing mkDotsParam) (do_nothing mkDotsStmt)
      (do_nothing mkIdent) expression (do_nothing mkFullType)
      (do_nothing mkTypeC) (do_nothing mkParam) (do_nothing mkDecl) statement
      (do_nothing mkTop) in
  combiner.V0.combiner_top_level

(* --------------------------------------------------------------------- *)
(* Traverse the hash tables and find corresponding context nodes that have
the same context children *)

(* this is just a sanity check - really only need to look at the top-level
   structure *)
let equal_mcode (_,_,info1) (_,_,info2) =
  Ast.get_offset info1 = Ast.get_offset info2

let equal_option e1 e2 =
  match (e1,e2) with
    (Some x, Some y) -> equal_mcode x y
  | (None, None) -> true
  | _ -> false

let dots fn d1 d2 =
  match (Ast0.unwrap d1,Ast0.unwrap d2) with
    (Ast0.DOTS(l1),Ast0.DOTS(l2)) -> List.for_all2 fn l1 l2
  | (Ast0.CIRCLES(l1),Ast0.CIRCLES(l2)) -> List.for_all2 fn l1 l2
  | (Ast0.STARS(l1),Ast0.STARS(l2)) -> List.for_all2 fn l1 l2
  | _ -> false

let rec equal_ident i1 i2 =
  match (Ast0.unwrap i1,Ast0.unwrap i2) with
    (Ast0.Id(name1),Ast0.Id(name2)) -> equal_mcode name1 name2
  | (Ast0.MetaId(name1),Ast0.MetaId(name2)) -> equal_mcode name1 name2
  | (Ast0.MetaFunc(name1),Ast0.MetaFunc(name2)) -> equal_mcode name1 name2
  | (Ast0.MetaLocalFunc(name1),Ast0.MetaLocalFunc(name2)) ->
      equal_mcode name1 name2
  | (Ast0.OptIdent(_),Ast0.OptIdent(_)) -> true
  | (Ast0.UniqueIdent(_),Ast0.UniqueIdent(_)) -> true
  | (Ast0.MultiIdent(_),Ast0.MultiIdent(_)) -> true
  | _ -> false

let rec equal_expression e1 e2 =
  match (Ast0.unwrap e1,Ast0.unwrap e2) with
    (Ast0.Ident(_),Ast0.Ident(_)) -> true
  | (Ast0.Constant(const1),Ast0.Constant(const2)) -> equal_mcode const1 const2
  | (Ast0.FunCall(_,lp1,_,rp1),Ast0.FunCall(_,lp2,_,rp2)) ->
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.Assignment(_,op1,_),Ast0.Assignment(_,op2,_)) ->
      equal_mcode op1 op2
  | (Ast0.CondExpr(_,why1,_,colon1,_),Ast0.CondExpr(_,why2,_,colon2,_)) ->
      equal_mcode why1 why2 && equal_mcode colon1 colon2
  | (Ast0.Postfix(_,op1),Ast0.Postfix(_,op2)) -> equal_mcode op1 op2
  | (Ast0.Infix(_,op1),Ast0.Infix(_,op2)) -> equal_mcode op1 op2
  | (Ast0.Unary(_,op1),Ast0.Unary(_,op2)) -> equal_mcode op1 op2
  | (Ast0.Binary(_,op1,_),Ast0.Binary(_,op2,_)) -> equal_mcode op1 op2
  | (Ast0.Paren(lp1,_,rp1),Ast0.Paren(lp2,_,rp2)) ->
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.ArrayAccess(_,lb1,_,rb1),Ast0.ArrayAccess(_,lb2,_,rb2)) ->
      equal_mcode lb1 lb2 && equal_mcode rb1 rb2
  | (Ast0.RecordAccess(_,pt1,_),Ast0.RecordAccess(_,pt2,_)) ->
      equal_mcode pt1 pt2
  | (Ast0.RecordPtAccess(_,ar1,_),Ast0.RecordPtAccess(_,ar2,_)) ->
      equal_mcode ar1 ar2
  | (Ast0.Cast(lp1,_,rp1,_),Ast0.Cast(lp2,_,rp2,_)) ->
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.MetaConst(name1,_),Ast0.MetaConst(name2,_))
  | (Ast0.MetaErr(name1),Ast0.MetaErr(name2))
  | (Ast0.MetaExpr(name1,_),Ast0.MetaExpr(name2,_))
  | (Ast0.MetaExprList(name1),Ast0.MetaExprList(name2)) ->
      equal_mcode name1 name2
  | (Ast0.EComma(cm1),Ast0.EComma(cm2)) -> equal_mcode cm1 cm2
  | (Ast0.DisjExpr(_),Ast0.DisjExpr(_)) -> true
  | (Ast0.NestExpr(_),Ast0.NestExpr(_)) -> true
  | (Ast0.Edots(dots1,_),Ast0.Edots(dots2,_))
  | (Ast0.Ecircles(dots1,_),Ast0.Ecircles(dots2,_))
  | (Ast0.Estars(dots1,_),Ast0.Estars(dots2,_)) -> equal_mcode dots1 dots2
  | (Ast0.OptExp(_),Ast0.OptExp(_)) -> true
  | (Ast0.UniqueExp(_),Ast0.UniqueExp(_)) -> true
  | (Ast0.MultiExp(_),Ast0.MultiExp(_)) -> true
  | _ -> false

let rec equal_fullType ft1 ft2 =
  match (Ast0.unwrap ft1,Ast0.unwrap ft2) with
    (Ast0.Type(cv1,_),Ast0.Type(cv2,_)) -> equal_option cv1 cv2
  | (Ast0.OptType(_),Ast0.OptType(_)) -> true
  | (Ast0.UniqueType(_),Ast0.UniqueType(_)) -> true
  | (Ast0.MultiType(_),Ast0.MultiType(_)) -> true
  | _ -> false

let rec equal_typeC t1 t2 =
  match (Ast0.unwrap t1,Ast0.unwrap t2) with
    (Ast0.BaseType(ty1,sign1),Ast0.BaseType(ty2,sign2)) ->
      equal_mcode ty1 ty2 && equal_option sign1 sign2
  | (Ast0.Pointer(_,star1),Ast0.Pointer(_,star2)) ->
      equal_mcode star1 star2
  | (Ast0.Array(_,lb1,_,rb1),Ast0.Array(_,lb2,_,rb2)) ->
      equal_mcode lb1 lb2 && equal_mcode rb1 rb2
  | (Ast0.StructUnionName(name1,kind1),Ast0.StructUnionName(name2,kind2)) ->
      equal_mcode name1 name2 && equal_mcode kind1 kind2
  | (Ast0.TypeName(name1),Ast0.TypeName(name2)) -> equal_mcode name1 name2
  | (Ast0.MetaType(name1),Ast0.MetaType(name2)) -> equal_mcode name1 name2
  | _ -> false

let rec equal_declaration d1 d2 =
  match (Ast0.unwrap d1,Ast0.unwrap d2) with
    (Ast0.Init(_,_,eq1,_,sem1),Ast0.Init(_,_,eq2,_,sem2)) ->
      equal_mcode eq1 eq2 && equal_mcode sem1 sem2
  | (Ast0.UnInit(_,_,sem1),Ast0.UnInit(_,_,sem2)) -> equal_mcode sem1 sem2
  | (Ast0.OptDecl(_),Ast0.OptDecl(_)) -> true
  | (Ast0.UniqueDecl(_),Ast0.UniqueDecl(_)) -> true
  | (Ast0.MultiDecl(_),Ast0.MultiDecl(_)) -> true
  | _ -> false
	
let rec equal_parameterTypeDef p1 p2 =
  match (Ast0.unwrap p1,Ast0.unwrap p2) with
    (Ast0.VoidParam(_),Ast0.VoidParam(_)) -> true
  | (Ast0.Param(_,_),Ast0.Param(_,_)) -> true
  | (Ast0.MetaParam(name1),Ast0.MetaParam(name2))
  | (Ast0.MetaParamList(name1),Ast0.MetaParamList(name2)) ->
      equal_mcode name1 name2
  | (Ast0.PComma(cm1),Ast0.PComma(cm2)) -> equal_mcode cm1 cm2
  | (Ast0.Pdots(dots1),Ast0.Pdots(dots2))
  | (Ast0.Pcircles(dots1),Ast0.Pcircles(dots2)) -> equal_mcode dots1 dots2
  | (Ast0.OptParam(_),Ast0.OptParam(_)) -> true
  | (Ast0.UniqueParam(_),Ast0.UniqueParam(_)) -> true
  | _ -> false

let rec equal_statement s1 s2 =
  match (Ast0.unwrap s1,Ast0.unwrap s2) with
    (Ast0.FunDecl(stg1,_,lp1,_,rp1,lbrace1,_,rbrace1),
     Ast0.FunDecl(stg2,_,lp2,_,rp2,lbrace2,_,rbrace2)) ->
       equal_option stg1 stg2 && equal_mcode lp1 lp2 && equal_mcode rp1 rp2 &&
       equal_mcode lbrace1 lbrace2 && equal_mcode rbrace1 rbrace2
  | (Ast0.Decl(_),Ast0.Decl(_)) -> true
  | (Ast0.Seq(lbrace1,_,rbrace1),Ast0.Seq(lbrace2,_,rbrace2)) ->
      equal_mcode lbrace1 lbrace2 && equal_mcode rbrace1 rbrace2
  | (Ast0.ExprStatement(_,sem1),Ast0.ExprStatement(_,sem2)) ->
      equal_mcode sem1 sem2
  | (Ast0.IfThen(iff1,lp1,_,rp1,_),Ast0.IfThen(iff2,lp2,_,rp2,_)) ->
      equal_mcode iff1 iff2 && equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.IfThenElse(iff1,lp1,_,rp1,_,els1,_),
     Ast0.IfThenElse(iff2,lp2,_,rp2,_,els2,_)) ->
       equal_mcode iff1 iff2 &&
	 equal_mcode lp1 lp2 && equal_mcode rp1 rp2 && equal_mcode els1 els2
  | (Ast0.While(whl1,lp1,_,rp1,_),Ast0.While(whl2,lp2,_,rp2,_)) ->
      equal_mcode whl1 whl2 && equal_mcode lp1 lp2 && equal_mcode rp1 rp2
  | (Ast0.Do(d1,_,whl1,lp1,_,rp1,sem1),Ast0.Do(d2,_,whl2,lp2,_,rp2,sem2)) ->
      equal_mcode whl1 whl2 && equal_mcode d1 d2 &&
      equal_mcode lp1 lp2 && equal_mcode rp1 rp2 && equal_mcode sem1 sem2
  | (Ast0.For(fr1,lp1,_,sem11,_,sem21,_,rp1,_),
     Ast0.For(fr2,lp2,_,sem12,_,sem22,_,rp2,_)) ->
       equal_mcode fr1 fr2 && equal_mcode lp1 lp2 &&
       equal_mcode sem11 sem21 && equal_mcode sem12 sem22 &&
       equal_mcode rp1 rp2
  | (Ast0.Return(ret1,sem1),Ast0.Return(ret2,sem2)) ->
      equal_mcode ret1 ret2 && equal_mcode sem1 sem2
  | (Ast0.ReturnExpr(ret1,_,sem1),Ast0.ReturnExpr(ret2,_,sem2)) ->
      equal_mcode ret1 ret2 && equal_mcode sem1 sem2
  | (Ast0.MetaStmt(name1),Ast0.MetaStmt(name2))
  | (Ast0.MetaStmtList(name1),Ast0.MetaStmtList(name2)) ->
      equal_mcode name1 name2
  | (Ast0.Disj(_),Ast0.Disj(_)) -> true
  | (Ast0.Nest(_),Ast0.Nest(_)) -> true
  | (Ast0.Exp(_),Ast0.Exp(_)) -> true
  | (Ast0.Dots(d1,_),Ast0.Dots(d2,_))
  | (Ast0.Circles(d1,_),Ast0.Circles(d2,_))
  | (Ast0.Stars(d1,_),Ast0.Stars(d2,_)) -> equal_mcode d1 d2
  | (Ast0.OptStm(_),Ast0.OptStm(_)) -> true
  | (Ast0.UniqueStm(_),Ast0.UniqueStm(_)) -> true
  | (Ast0.MultiStm(_),Ast0.MultiStm(_)) -> true
  | _ -> false
	
let rec equal_top_level t1 t2 =
  match (Ast0.unwrap t1,Ast0.unwrap t2) with
    (Ast0.DECL(_),Ast0.DECL(_)) -> true
  | (Ast0.INCLUDE(inc1,name1),Ast0.INCLUDE(inc2,name2)) ->
      equal_mcode inc1 inc2 && equal_mcode name1 name2
  | (Ast0.FILEINFO(old_file1,new_file1),Ast0.FILEINFO(old_file2,new_file2)) ->
      equal_mcode old_file1 old_file2 && equal_mcode new_file1 new_file2
  | (Ast0.FUNCTION(_),Ast0.FUNCTION(_)) -> true
  | (Ast0.CODE(_),Ast0.CODE(_)) -> true
  | (Ast0.ERRORWORDS(_),Ast0.ERRORWORDS(_)) -> true
  | _ -> false

let root_equal e1 e2 =
  match (e1,e2) with
    (DotsExpr(d1),DotsExpr(d2)) -> dots equal_expression d1 d2
  | (DotsParam(d1),DotsParam(d2)) -> dots equal_parameterTypeDef d1 d2
  | (DotsStmt(d1),DotsStmt(d2)) -> dots equal_statement d1 d2
  | (Ident(i1),Ident(i2)) -> equal_ident i1 i2
  | (Expr(e1),Expr(e2)) -> equal_expression e1 e2
  | (FullType(f1),FullType(f2)) -> equal_fullType f1 f2
  | (TypeC(t1),TypeC(t2)) -> equal_typeC t1 t2
  | (Param(p1),Param(p2)) -> equal_parameterTypeDef p1 p2
  | (Decl(d1),Decl(d2)) -> equal_declaration d1 d2
  | (Stmt(s1),Stmt(s2)) -> equal_statement s1 s2
  | (Top(t1),Top(t2)) -> equal_top_level t1 t2
  | _ -> false

let traverse minus_table plus_table =
  Hashtbl.iter
    (function key ->
      function (e,l) ->
	try
	  let (plus_e,plus_l) = Hashtbl.find plus_table key in
	  if root_equal e plus_e && Common.equal_set l plus_l
	  then (set_tinfo e Ast0.BindContext;
		set_tinfo plus_e Ast0.BindContext)
	with Not_found -> ())
    minus_table

(* --------------------------------------------------------------------- *)

let context_neg minus plus =
  let minus_table =
    (Hashtbl.create(50) :
       (Ast.mcodekind list, anything * int list) Hashtbl.t) in
  let plus_table =
    (Hashtbl.create(50) :
       (Ast.mcodekind list, anything * int list) Hashtbl.t) in
  List.iter2
    (function minus -> function plus ->
      Hashtbl.clear minus_table;
      Hashtbl.clear plus_table;
      let _ = classify minus_table minus in
      let _ = classify plus_table plus in
      traverse minus_table plus_table)
    minus plus
