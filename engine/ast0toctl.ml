(* Question: where do we put the existential quantifier for or.  At the
moment, let it float inwards. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module CTL = Ast_ctl

let warning s = Printf.fprintf stderr "warning: %s\n" s

(* --------------------------------------------------------------------- *)

type predicate =
    TrueBranch | FalseBranch | After
  | Paren of string
  | Match of Ast.rule_elem * string
  | MatchModif of Ast.rule_elem * string

let texify s =
  let len = String.length s in
  let rec loop n =
    if n = len
    then ""
    else
      match String.get s n with
	'_' -> Printf.sprintf "\\_%s" (loop (n+1))
      | '{' -> Printf.sprintf "{\\ttlb}%s" (loop (n+1))
      | '}' -> Printf.sprintf "{\\ttrb}%s" (loop (n+1))
      | '>' -> Printf.sprintf "\\mth{>}%s" (loop (n+1))
      | c -> Printf.sprintf "%c%s" c (loop (n+1)) in
  Printf.sprintf "\\mita{%s}" (loop 0)

let pred2c = function
    TrueBranch -> "\\msf{TrueBranch}"
  | FalseBranch -> "\\msf{FalseBranch}"
  | After -> "\\msf{After}"
  | Paren(s) -> "\\msf{Paren}("^s^")"
  | Match(re,v)
  | MatchModif(re,v) ->
      Printf.sprintf "%s_{%s}"
	(texify(Unparse_cocci.rule_elem_to_string re))
	v

(* --------------------------------------------------------------------- *)

let get_option fn = function
    None -> None
  | Some x -> Some (fn x)

let get_list_option fn = function
    None -> []
  | Some x -> fn x

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

let dots fn l after =
  match l with
    Ast0.DOTS(x) ->
      let rec loop = function
	  [] -> (match after with Some x -> x | None -> CTL.True)
	| [x] -> fn x after
	| x::xs -> fn x (Some(loop xs)) in
      loop x
  | Ast0.CIRCLES(x) -> failwith "not supported"
  | Ast0.STARS(x) -> failwith "not supported"

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let ctr = ref 0
let fresh_var _ =
  let c = !ctr in
  ctr := !ctr + 1;
  Printf.sprintf "v%d" c

let make_seq first = function
    None -> first
  | Some rest -> CTL.And(first,CTL.AX(rest))

let make_match code =
  let v = fresh_var() in
  if Ast.contains_modif code
  then CTL.Exists(v,CTL.Pred(MatchModif(code,v)))
  else CTL.Exists(v,CTL.Pred(Match(code,v)))

let rec statement stmt after =
  match stmt with
    Ast0.Decl(decl) ->
      make_seq (make_match(Ast.Decl(Ast0toast.declaration decl))) after
  | Ast0.Seq(lbrace,body,rbrace) ->
      let v = fresh_var() in
      let start_brace =
	CTL.And(make_match(Ast.SeqStart(Ast0toast.mcode lbrace)),
		CTL.Pred(Paren v)) in
      let end_brace =
	CTL.And
	  (make_match(Ast.SeqEnd(Ast0toast.mcode rbrace)),
	   CTL.Pred(Paren v)) in
      make_seq start_brace
	(Some(dots statement body (Some (make_seq end_brace after))))
  | Ast0.ExprStatement(exp,sem) ->
      make_seq
	(make_match
	   (Ast.ExprStatement (Ast0toast.expression exp,Ast0toast.mcode sem)))
	after
  | Ast0.IfThen(iff,lp,exp,rp,branch) ->
      let if_header =
	make_match
	  (Ast.IfHeader
	     (Ast0toast.mcode iff,Ast0toast.mcode lp,
	       Ast0toast.expression exp,Ast0toast.mcode rp)) in
      let then_line =
	CTL.Implies(CTL.Pred(TrueBranch),statement branch None) in
      let else_line = CTL.Implies(CTL.Pred(FalseBranch),CTL.False) in
      let after_line =
	match after with
	  None -> CTL.True
	| Some after ->	CTL.Implies(CTL.Pred(After),after) in
      make_seq if_header
	  (Some(CTL.And (CTL.And(then_line,else_line),after_line)))
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
      let if_header =
	make_match
	  (Ast.IfHeader
	     (Ast0toast.mcode iff,Ast0toast.mcode lp,
	       Ast0toast.expression exp,Ast0toast.mcode rp)) in
      let then_line =
	CTL.Implies(CTL.Pred(TrueBranch),statement branch1 None) in
      let else_line =
	CTL.Implies(CTL.Pred(FalseBranch),statement branch2 None) in
      let after_line =
	match after with
	  None -> CTL.True
	| Some after ->	CTL.Implies(CTL.Pred(After),after) in
      make_seq if_header
	  (Some(CTL.And (CTL.And(then_line,else_line),after_line)))
  | Ast0.While(wh,lp,exp,rp,body) ->
      let while_header =
	make_match
	  (Ast.WhileHeader
	     (Ast0toast.mcode wh,Ast0toast.mcode lp,
	      Ast0toast.expression exp,Ast0toast.mcode rp)) in
      let body_line = CTL.Implies(CTL.Pred(TrueBranch),statement body None) in
      let after_line =
	match after with
	  None -> CTL.True
	| Some after -> CTL.Implies(CTL.Pred(FalseBranch),after) in
      make_seq while_header (Some (CTL.And(body_line,after_line)))
  | Ast0.Return(ret,sem) ->
      make_seq
	(make_match(Ast.Return(Ast0toast.mcode ret,Ast0toast.mcode sem)))
	after
  | Ast0.ReturnExpr(ret,exp,sem) ->
      let return = 
	Ast.ReturnExpr
	  (Ast0toast.mcode ret,Ast0toast.expression exp,Ast0toast.mcode sem) in
      make_seq (make_match return) after
  | Ast0.MetaStmt(name) ->
      make_seq (make_match(Ast.MetaStmt(Ast0toast.mcode name))) after
  | Ast0.MetaStmtList(name) ->
      make_seq (make_match(Ast.MetaStmtList(Ast0toast.mcode name))) after
  | Ast0.Exp(exp) ->
      make_seq (make_match(Ast.Exp(Ast0toast.expression exp))) after
  | Ast0.Disj(rule_elem_dots_list) ->
      List.fold_right
	(function cur ->
	  function rest -> CTL.Or(dots statement cur None,rest))
	rule_elem_dots_list CTL.False
  | Ast0.Nest(rule_elem_dots) ->
      let dots_pattern = dots statement rule_elem_dots None in
      (match after with
	None ->
	  CTL.AG(CTL.Or(dots_pattern,CTL.Not dots_pattern))
      |	Some after ->
	  CTL.AU(CTL.Or(dots_pattern,CTL.Not dots_pattern),after))
  | Ast0.Dots(_,None)    ->
      (match after with
	None -> CTL.True
      |	Some after -> CTL.AF(after))
  | Ast0.Dots(_,Some statement_dots)    ->
      (match after with
	None ->
	  CTL.AG(CTL.Not(dots statement statement_dots None))
      |	Some after ->
	  CTL.AU(CTL.Not(dots statement statement_dots None),after))
  | Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace) ->
      let stg = get_option Ast0toast.mcode stg in
      let name = Ast0toast.ident name in
      let lp = Ast0toast.mcode lp in
      let params = Ast0toast.parameter_list params in
      let rp = Ast0toast.mcode rp in
      let function_header =
	make_match(Ast.FunDecl(stg,name,lp,params,rp)) in
      let v = fresh_var() in
      let start_brace =
	CTL.And(make_match(Ast.SeqStart(Ast0toast.mcode lbrace)),
		CTL.Pred(Paren v)) in
      let end_brace =
	CTL.And
	  (make_match(Ast.SeqEnd(Ast0toast.mcode rbrace)),
	   CTL.Pred(Paren v)) in
      make_seq function_header
	(Some
	   (make_seq start_brace
	      (Some(dots statement body (Some(make_seq end_brace after))))))
  | Ast0.OptStm(stm) ->
      (* doesn't work for ?f(); f();, ie when the optional thing is the same
	 as the thing that comes immediately after *)
      let pattern = statement stm after in
      (match after with
	None -> CTL.Or(pattern,CTL.Not pattern)
      |	Some after ->
	  CTL.Or(pattern,CTL.And(CTL.Not(statement stm None),after)))
  | Ast0.UniqueStm(stm) ->
      warning "arities not yet supported"; statement stm after
  | Ast0.MultiStm(stm) ->
      warning "arities not yet supported"; statement stm after
  | _ -> failwith "not supported"

(* --------------------------------------------------------------------- *)

let fvdots fn = function
    Ast.DOTS(l) | Ast.CIRCLES(l) | Ast.STARS(l) ->
      List.fold_left Common.union_set [] (List.map fn l)

let mcode (x,_) = x

let rec fvident = function
    Ast.Id(name) -> []
  | Ast.MetaId(name) -> [mcode name]
  | Ast.MetaFunc(name) -> [mcode name]
  | Ast.MetaLocalFunc(name) -> [mcode name]
  | Ast.OptIdent(id) -> fvident id
  | Ast.UniqueIdent(id) -> fvident id
  | Ast.MultiIdent(id) -> fvident id

let rec fvexpr = function
    Ast.Ident(id) -> ((fvident id) : string list)
  | Ast.Constant(const) -> []
  | Ast.FunCall(fn,lp,args,rp) ->
      Common.union_set (fvexpr fn) (fvdots fvexpr args)
  | Ast.Assignment(left,op,right) ->
      Common.union_set (fvexpr left) (fvexpr right)
  | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
      Common.union_set (fvexpr exp1)
	(Common.union_set (get_list_option fvexpr exp2) (fvexpr exp3))
  | Ast.Postfix(exp,op) -> fvexpr exp
  | Ast.Infix(exp,op) -> fvexpr exp
  | Ast.Unary(exp,op) -> fvexpr exp
  | Ast.Binary(left,op,right) ->
      Common.union_set (fvexpr left) (fvexpr right)
  | Ast.Paren(lp,exp,rp) -> fvexpr exp
  | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
      Common.union_set (fvexpr exp1) (fvexpr exp2)
  | Ast.RecordAccess(exp,pt,field) ->
      Common.union_set (fvexpr exp) (fvident field)
  | Ast.RecordPtAccess(exp,ar,field) ->
      Common.union_set (fvexpr exp) (fvident field)
  | Ast.Cast(lp,ty,rp,exp) -> Common.union_set (fvtypeC ty) (fvexpr exp)
  | Ast.MetaConst(name,_) -> [mcode name]
  | Ast.MetaErr(name) -> [mcode name]
  | Ast.MetaExpr(name,_) -> [mcode name]
  | Ast.MetaExprList(name) -> [mcode name]
  | Ast.EComma(cm) -> []
  | Ast.DisjExpr(exp_list) ->
      List.fold_left Common.union_set [] (List.map fvexpr exp_list)
  | Ast.NestExpr(expr_dots) -> fvdots fvexpr expr_dots
  | Ast.Edots(dots,Some whencode)
  | Ast.Ecircles(dots,Some whencode)
  | Ast.Estars(dots,Some whencode) -> fvexpr whencode
  | Ast.Edots(dots,None) | Ast.Ecircles(dots,None)
  | Ast.Estars(dots,None) -> []
  | Ast.OptExp(exp) -> fvexpr exp
  | Ast.UniqueExp(exp) -> fvexpr exp
  | Ast.MultiExp(exp) -> fvexpr exp

and fvtypeC = function
    Ast.BaseType(ty,_) -> []
  | Ast.Pointer(ty,star) -> fvtypeC ty
  | Ast.Array(ty,lb,None,rb) -> fvtypeC ty
  | Ast.Array(ty,lb,Some size,rb) ->
      Common.union_set (fvtypeC ty) (fvexpr size)
  | Ast.StructUnionName(name,kind) -> []
  | Ast.TypeName(name)-> []
  | Ast.MetaType(name)-> [mcode name]
  | Ast.OptType(ty) -> fvtypeC ty
  | Ast.UniqueType(ty) -> fvtypeC ty
  | Ast.MultiType(ty) -> fvtypeC ty

let rec fvdeclaration = function
    Ast.Init(ty,id,eq,exp,sem) ->
      Common.union_set (fvtypeC ty)
	(Common.union_set (fvident id) (fvexpr exp))
  | Ast.UnInit(ty,id,sem) -> Common.union_set (fvtypeC ty) (fvident id)
  | Ast.OptDecl(decl) -> fvdeclaration decl
  | Ast.UniqueDecl(decl) -> fvdeclaration decl
  | Ast.MultiDecl(decl) -> fvdeclaration decl

let rec fvparameterTypeDef = function
    Ast.VoidParam(ty) -> fvtypeC ty
  | Ast.Param(id,_,ty) -> Common.union_set (fvtypeC ty) (fvident id)
  | Ast.MetaParam(name) -> [mcode name]
  | Ast.MetaParamList(name) -> [mcode name]
  | Ast.PComma(cm) -> []
  | Ast.Pdots(dots) | Ast.Pcircles(dots) -> []
  | Ast.OptParam(param) -> fvparameterTypeDef param
  | Ast.UniqueParam(param) -> fvparameterTypeDef param

let fvparameter_list = fvdots fvparameterTypeDef

let rec fvrule_elem = function
    Ast.FunDecl(stg,name,lp,params,rp) -> fvparameter_list params
  | Ast.Decl(decl) -> fvdeclaration decl
  | Ast.SeqStart(brace) -> []
  | Ast.SeqEnd(brace) -> []
  | Ast.ExprStatement(exp,sem) -> fvexpr exp
  | Ast.IfHeader(iff,lp,exp,rp) -> fvexpr exp
  | Ast.Else(els) -> []
  | Ast.WhileHeader(whl,lp,exp,rp) -> fvexpr exp
  | Ast.Do(d) -> []
  | Ast.WhileTail(whl,lp,exp,rp,sem) -> fvexpr exp
  | Ast.ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp) ->
      Common.union_set (get_list_option fvexpr e1)
	(Common.union_set (get_list_option fvexpr e2)
	   (get_list_option fvexpr e3))
  | Ast.Return(ret,sem) -> []
  | Ast.ReturnExpr(ret,exp,sem) -> fvexpr exp
  | Ast.MetaStmt(name) -> [mcode name]
  | Ast.MetaStmtList(name) -> [mcode name]
  | Ast.Disj(rule_elem_dots_list) ->
      List.fold_left Common.union_set []
	(List.map (function x -> fvdots fvrule_elem x) rule_elem_dots_list)
  | Ast.Nest(rule_elem_dots) -> fvdots fvrule_elem rule_elem_dots
  | Ast.Exp(exp) -> fvexpr exp
  | Ast.Dots(dots,None) | Ast.Circles(dots,None) | Ast.Stars(dots,None) -> []
  | Ast.Dots(d,Some x) | Ast.Circles(d,Some x) | Ast.Stars(d,Some x) ->
      fvdots fvrule_elem x
  | Ast.OptRuleElem(re) ->
      List.fold_left Common.union_set [] (List.map fvrule_elem re)
  | Ast.UniqueRuleElem(re) ->
      List.fold_left Common.union_set [] (List.map fvrule_elem re)
  | Ast.MultiRuleElem(re) ->
      List.fold_left Common.union_set [] (List.map fvrule_elem re)

let free_table =
  (Hashtbl.create(50) :
     ((predicate,string) CTL.generic_ctl,string list) Hashtbl.t)

let rec free_vars x =
  let res =
    match x with
      CTL.False -> []
    | CTL.True -> []
    | CTL.Pred(Match(p,_)) -> fvrule_elem p
    | CTL.Pred(MatchModif(p,_)) -> fvrule_elem p
    | CTL.Pred(Paren(p)) -> [p]
    | CTL.Pred(p) -> []
    | CTL.Not(f) -> free_vars f
    | CTL.Exists(vars,f) -> free_vars f
    | CTL.And(f1,f2) -> Common.union_set (free_vars f1) (free_vars f2)
    | CTL.Or(f1,f2) -> Common.union_set (free_vars f1) (free_vars f2)
    | CTL.Implies(f1,f2) -> Common.union_set (free_vars f1) (free_vars f2)
    | CTL.AF(f) -> free_vars f
    | CTL.AX(f) -> free_vars f
    | CTL.AG(f) -> free_vars f
    | CTL.AU(f1,f2) -> Common.union_set (free_vars f1) (free_vars f2)
    | CTL.EF(f) -> free_vars f
    | CTL.EX(f) -> free_vars f
    | CTL.EG(f) -> free_vars f
    | CTL.EU(f1,f2) -> Common.union_set (free_vars f1) (free_vars f2) in
  Hashtbl.add free_table x res;
  res

(* --------------------------------------------------------------------- *)

let add_quants formula variables =
  List.fold_left (function prev -> function v -> CTL.Exists (v,prev))
    formula variables

let rec add_quantifiers quantified = function
    CTL.False -> CTL.False
  | CTL.True -> CTL.True
  | CTL.Pred(Match(p,_))
  | CTL.Pred(MatchModif(p,_)) as x ->
      let vars = Hashtbl.find free_table x in
      let fresh =
	List.filter (function x -> not (List.mem x quantified)) vars in
      add_quants x fresh
  | CTL.Pred(p) -> CTL.Pred(p)
  | CTL.Not(f) -> CTL.Not(add_quantifiers quantified f)
  | CTL.Exists(vars,f) -> CTL.Exists(vars,add_quantifiers quantified f)
  | CTL.And(f1,f2) ->
      binop f1 f2 quantified (function x -> function y -> CTL.And(x,y))
  | CTL.Or(f1,f2) ->
      CTL.Or(add_quantifiers quantified f1,add_quantifiers quantified f2)
  | CTL.Implies(f1,f2) ->
      binop f1 f2 quantified (function x -> function y -> CTL.Implies(x,y))
  | CTL.AF(f) -> CTL.AF(add_quantifiers quantified f)
  | CTL.AX(f) -> CTL.AX(add_quantifiers quantified f)
  | CTL.AG(f) -> CTL.AG(add_quantifiers quantified f)
  | CTL.AU(f1,f2) ->
      binop f1 f2 quantified (function x -> function y -> CTL.AU(x,y))
  | CTL.EF(f) -> CTL.EF(add_quantifiers quantified f)
  | CTL.EX(f) -> CTL.EX(add_quantifiers quantified f)
  | CTL.EG(f) -> CTL.EG(add_quantifiers quantified f)
  | CTL.EU(f1,f2) ->
      CTL.EU(add_quantifiers quantified f1,add_quantifiers quantified f2)

and binop f1 f2 quantified fn =
  let f1vars = Hashtbl.find free_table f1 in
  let f2vars = Hashtbl.find free_table f2 in
  let common = Common.inter_set f1vars f2vars in
  let fresh = Common.minus_set common quantified in
  let now_quantified = fresh @ quantified in
  add_quants
    (fn
       (add_quantifiers now_quantified f1)
       (add_quantifiers now_quantified f2))
    fresh

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let top_level = function
    Ast0.DECL(decl) -> failwith "not supported"
  | Ast0.INCLUDE(inc,s) -> failwith "not supported"
  | Ast0.FILEINFO(old_file,new_file) -> failwith "not supported"
  | Ast0.FUNCTION(stmt) ->
      let start = statement stmt None in
      let _ = free_vars start in
      add_quantifiers [] start
  | Ast0.CODE(rule_elem_dots) ->
      let start = dots statement rule_elem_dots None in
      let _ = free_vars start in
      add_quantifiers [] start
  | Ast0.ERRORWORDS(exps) -> failwith "not supported"
  | Ast0.OTHER(_) -> failwith "eliminated by top_level"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let ast0toctl l =
  ctr := 0;
  List.map top_level l
