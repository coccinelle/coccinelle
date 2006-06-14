(* Question: where do we put the existential quantifier for or.  At the
moment, let it float inwards. *)

module Ast = Ast_cocci
(* module Ast0 = Ast0_cocci *)
(* module CTL = Ast_ctl *)

let warning s = Printf.fprintf stderr "warning: %s\n" s

(* --------------------------------------------------------------------- *)

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
    Lib_engine.TrueBranch -> "\\msf{Lib_engine.TrueBranch}"
  | Lib_engine.FalseBranch -> "\\msf{Lib_engine.FalseBranch}"
  | Lib_engine.After -> "\\msf{Lib_engine.After}"
  | Lib_engine.Paren(s) -> "\\msf{Lib_engine.Paren}("^s^")"
  | Lib_engine.Match(re) ->
      Printf.sprintf "%s" (texify(Unparse_cocci.rule_elem_to_string re))

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
  match Ast0_cocci.unwrap l with
    Ast0_cocci.DOTS(x) ->
      let rec loop = function
	  [] -> (match after with Some x -> x | None -> Ast_ctl.True)
	| [x] -> fn x after
	| x::xs -> fn x (Some(loop xs)) in
      loop x
  | Ast0_cocci.CIRCLES(x) -> failwith "not supported"
  | Ast0_cocci.STARS(x) -> failwith "not supported"

(* --------------------------------------------------------------------- *)
(* Whenify *)
(* For A ... B, neither A nor B should occur in the code matched by the ...
We add these to any when code associated with the dots *)

let rec when_dots before after d =
  Ast0_cocci.rewrap d
    (match Ast0_cocci.unwrap d with
      Ast0_cocci.DOTS(l) -> Ast0_cocci.DOTS(dots_list before after l)
    | Ast0_cocci.CIRCLES(l) -> Ast0_cocci.CIRCLES(dots_list before after l)
    | Ast0_cocci.STARS(l) -> Ast0_cocci.STARS(dots_list before after l))

and dots_list before after = function
    [] -> []
  | [x] -> [when_statement before after x]
  | (cur::((aft::_) as rest)) ->
      (when_statement before (Some aft) cur)::
      (dots_list (Some cur) after rest)

and when_statement before after s =
  Ast0_cocci.rewrap s
    (match Ast0_cocci.unwrap s with
      Ast0_cocci.Decl(decl) as x -> x
    | Ast0_cocci.Seq(lbrace,body,rbrace) ->
	Ast0_cocci.Seq(lbrace,when_dots None None body,rbrace)
    | Ast0_cocci.ExprStatement(exp,sem) as x -> x
    | Ast0_cocci.IfThen(iff,lp,exp,rp,branch) ->
	Ast0_cocci.IfThen(iff,lp,exp,rp,when_statement None None branch)
    | Ast0_cocci.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
	Ast0_cocci.IfThenElse(iff,lp,exp,rp,
	  when_statement None None branch1,els,
	  when_statement None None branch2)
    | Ast0_cocci.While(wh,lp,exp,rp,body) ->
	Ast0_cocci.While(wh,lp,exp,rp,when_statement None None body)
    | Ast0_cocci.Return(ret,sem) as x -> x
    | Ast0_cocci.ReturnExpr(ret,exp,sem) as x -> x
    | Ast0_cocci.MetaStmt(name) as x -> x
    | Ast0_cocci.MetaStmtList(name) as x -> x
    | Ast0_cocci.Exp(exp) as x -> x
    | Ast0_cocci.Disj(rule_elem_dots_list) ->
	Ast0_cocci.Disj(List.map (when_dots before after) rule_elem_dots_list)
    | Ast0_cocci.Nest(rule_elem_dots) ->
	Ast0_cocci.Nest(when_dots None None rule_elem_dots)
    | Ast0_cocci.Dots(d,None) as x ->
	(* don't care about line numbers any more, and never cared in when *)
	(match (before,after) with
	  (None,None) -> x
	| (None,Some aft) -> Ast0_cocci.Dots(d,Some(Ast0_cocci.wrap(Ast0_cocci.DOTS([aft]))))
	| (Some bef,None) -> Ast0_cocci.Dots(d,Some(Ast0_cocci.wrap(Ast0_cocci.DOTS([bef]))))
	| (Some bef,Some aft) ->
	    let new_when =
	      Ast0_cocci.wrap(Ast0_cocci.Disj([Ast0_cocci.wrap(Ast0_cocci.DOTS([bef]));
				    Ast0_cocci.wrap(Ast0_cocci.DOTS([aft]))])) in
	    Ast0_cocci.Dots(d,Some(Ast0_cocci.wrap(Ast0_cocci.DOTS([new_when])))))
    | Ast0_cocci.Dots(d,Some statement_dots) as x ->
	(* don't care about line numbers any more, and never cared in when *)
	(match (before,after) with
	  (None,None) -> x
	| (None,Some aft) ->
	    let new_when =
	      Ast0_cocci.wrap
		(Ast0_cocci.Disj([Ast0_cocci.wrap(Ast0_cocci.DOTS([aft]));statement_dots])) in
	    Ast0_cocci.Dots(d,Some(Ast0_cocci.wrap(Ast0_cocci.DOTS([new_when]))))
	| (Some bef,None) ->
	    let new_when =
	      Ast0_cocci.wrap
		(Ast0_cocci.Disj([Ast0_cocci.wrap(Ast0_cocci.DOTS([bef]));statement_dots])) in
	    Ast0_cocci.Dots(d,Some(Ast0_cocci.wrap(Ast0_cocci.DOTS([new_when]))))
	| (Some bef,Some aft) ->
	    let new_when =
	      Ast0_cocci.wrap(Ast0_cocci.Disj([Ast0_cocci.wrap(Ast0_cocci.DOTS([bef]));
				    Ast0_cocci.wrap(Ast0_cocci.DOTS([aft]));
				    statement_dots])) in
	    Ast0_cocci.Dots(d,Some(Ast0_cocci.wrap(Ast0_cocci.DOTS([new_when])))))
    | Ast0_cocci.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace) ->
	Ast0_cocci.FunDecl(stg,name,lp,params,rp,lbrace,when_dots None None body,
		     rbrace)
    | Ast0_cocci.OptStm(stm) -> Ast0_cocci.OptStm(when_statement None None stm)
    | Ast0_cocci.UniqueStm(stm) -> Ast0_cocci.UniqueStm(when_statement None None stm)
    | Ast0_cocci.MultiStm(stm) -> Ast0_cocci.MultiStm(when_statement None None stm)
    | _ -> failwith "not supported")

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let ctr = ref 0
let fresh_var _ =
  let c = !ctr in
  ctr := !ctr + 1;
  Printf.sprintf "v%d" c

let make_seq first = function
    None -> first
  | Some rest -> Ast_ctl.And(first,Ast_ctl.AX(rest))

let make_match code =
  let v = fresh_var() in
  if Ast.contains_modif code
  then Ast_ctl.Pred(Lib_engine.Match(code),Ast_ctl.Modif v)
  else Ast_ctl.Pred(Lib_engine.Match(code),Ast_ctl.UnModif v)

let rec statement stmt after =
  match Ast0_cocci.unwrap stmt with
    Ast0_cocci.Decl(decl) ->
      make_seq (make_match(Ast.Decl(Ast0toast.declaration decl))) after
  | Ast0_cocci.Seq(lbrace,body,rbrace) ->
      let v = fresh_var() in
      let start_brace =
	Ast_ctl.And(make_match(Ast.SeqStart(Ast0toast.mcode lbrace)),
		Ast_ctl.Pred(Lib_engine.Paren v,Ast_ctl.Control)) in
      let end_brace =
	Ast_ctl.And
	  (make_match(Ast.SeqEnd(Ast0toast.mcode rbrace)),
	   Ast_ctl.Pred(Lib_engine.Paren v,Ast_ctl.Control)) in
      make_seq start_brace
	(Some(dots statement body (Some (make_seq end_brace after))))
  | Ast0_cocci.ExprStatement(exp,sem) ->
      make_seq
	(make_match
	   (Ast.ExprStatement (Ast0toast.expression exp,Ast0toast.mcode sem)))
	after
  | Ast0_cocci.IfThen(iff,lp,exp,rp,branch) ->
      let if_header =
	make_match
	  (Ast.IfHeader
	     (Ast0toast.mcode iff,Ast0toast.mcode lp,
	       Ast0toast.expression exp,Ast0toast.mcode rp)) in
      let then_line =
	Ast_ctl.Implies(Ast_ctl.Pred(Lib_engine.TrueBranch,Ast_ctl.Control),statement branch None) in
      let else_line =
	Ast_ctl.Implies(Ast_ctl.Pred(Lib_engine.FalseBranch,Ast_ctl.Control),Ast_ctl.False) in
      let after_line =
	match after with
	  None -> Ast_ctl.True
	| Some after ->	Ast_ctl.Implies(Ast_ctl.Pred(Lib_engine.After,Ast_ctl.Control),after) in
      make_seq if_header
	  (Some(Ast_ctl.And (Ast_ctl.And(then_line,else_line),after_line)))
  | Ast0_cocci.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
      let if_header =
	make_match
	  (Ast.IfHeader
	     (Ast0toast.mcode iff,Ast0toast.mcode lp,
	       Ast0toast.expression exp,Ast0toast.mcode rp)) in
      let then_line =
	Ast_ctl.Implies(Ast_ctl.Pred(Lib_engine.TrueBranch,Ast_ctl.Control),
		    statement branch1 None) in
      let else_line =
	Ast_ctl.Implies(Ast_ctl.Pred(Lib_engine.FalseBranch,Ast_ctl.Control),
		    statement branch2 None) in
      let after_line =
	match after with
	  None -> Ast_ctl.True
	| Some after ->	Ast_ctl.Implies(Ast_ctl.Pred(Lib_engine.After,Ast_ctl.Control),after) in
      make_seq if_header
	  (Some(Ast_ctl.And (Ast_ctl.And(then_line,else_line),after_line)))
  | Ast0_cocci.While(wh,lp,exp,rp,body) ->
      let while_header =
	make_match
	  (Ast.WhileHeader
	     (Ast0toast.mcode wh,Ast0toast.mcode lp,
	      Ast0toast.expression exp,Ast0toast.mcode rp)) in
      let body_line =
	Ast_ctl.Implies(Ast_ctl.Pred(Lib_engine.TrueBranch,Ast_ctl.Control),statement body None) in
      let after_line =
	match after with
	  None -> Ast_ctl.True
	| Some after -> Ast_ctl.Implies(Ast_ctl.Pred(Lib_engine.FalseBranch,Ast_ctl.Control),after) in
      make_seq while_header (Some (Ast_ctl.And(body_line,after_line)))
  | Ast0_cocci.Return(ret,sem) ->
      make_seq
	(make_match(Ast.Return(Ast0toast.mcode ret,Ast0toast.mcode sem)))
	after
  | Ast0_cocci.ReturnExpr(ret,exp,sem) ->
      let return = 
	Ast.ReturnExpr
	  (Ast0toast.mcode ret,Ast0toast.expression exp,Ast0toast.mcode sem) in
      make_seq (make_match return) after
  | Ast0_cocci.MetaStmt(name) ->
      make_seq (make_match(Ast.MetaStmt(Ast0toast.mcode name))) after
  | Ast0_cocci.MetaStmtList(name) ->
      make_seq (make_match(Ast.MetaStmtList(Ast0toast.mcode name))) after
  | Ast0_cocci.Exp(exp) ->
      make_seq (make_match(Ast.Exp(Ast0toast.expression exp))) after
  | Ast0_cocci.Disj(rule_elem_dots_list) ->
      let rec loop = function
	  [] -> Ast_ctl.False
	| [cur] -> dots statement cur None
	| cur::rest -> Ast_ctl.Or(dots statement cur None,loop rest) in
      loop rule_elem_dots_list
  | Ast0_cocci.Nest(rule_elem_dots) ->
      let dots_pattern = dots statement rule_elem_dots None in
      (match after with
	None ->
	  Ast_ctl.AG(Ast_ctl.Or(dots_pattern,Ast_ctl.Not dots_pattern))
      |	Some after ->
	  Ast_ctl.AU(Ast_ctl.Or(dots_pattern,Ast_ctl.Not dots_pattern),after))
  | Ast0_cocci.Dots(_,None)    ->
      (match after with
	None -> Ast_ctl.True
      |	Some after -> Ast_ctl.AF(after))
  | Ast0_cocci.Dots(_,Some statement_dots)    ->
      (match after with
	None ->
	  Ast_ctl.AG(Ast_ctl.Not(dots statement statement_dots None))
      |	Some after ->
	  Ast_ctl.AU(Ast_ctl.Not(dots statement statement_dots None),after))
  | Ast0_cocci.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace) ->
      let stg = get_option Ast0toast.mcode stg in
      let name = Ast0toast.ident name in
      let lp = Ast0toast.mcode lp in
      let params = Ast0toast.parameter_list params in
      let rp = Ast0toast.mcode rp in
      let function_header =
	make_match(Ast.FunDecl(stg,name,lp,params,rp)) in
      let v = fresh_var() in
      let start_brace =
	Ast_ctl.And(make_match(Ast.SeqStart(Ast0toast.mcode lbrace)),
		Ast_ctl.Pred(Lib_engine.Paren v,Ast_ctl.Control)) in
      let end_brace =
	Ast_ctl.And
	  (make_match(Ast.SeqEnd(Ast0toast.mcode rbrace)),
	   Ast_ctl.Pred(Lib_engine.Paren v,Ast_ctl.Control)) in
      make_seq function_header
	(Some
	   (make_seq start_brace
	      (Some(dots statement body (Some(make_seq end_brace after))))))
  | Ast0_cocci.OptStm(stm) ->
      (* doesn't work for ?f(); f();, ie when the optional thing is the same
	 as the thing that comes immediately after *)
      let pattern = statement stm after in
      (match after with
	None -> Ast_ctl.Or(pattern,Ast_ctl.Not pattern)
      |	Some after ->
	  Ast_ctl.Or(pattern,Ast_ctl.And(Ast_ctl.Not(statement stm None),after)))
  | Ast0_cocci.UniqueStm(stm) ->
      warning "arities not yet supported"; statement stm after
  | Ast0_cocci.MultiStm(stm) ->
      warning "arities not yet supported"; statement stm after
  | _ -> failwith "not supported"

(* --------------------------------------------------------------------- *)

let fvdots fn = function
    Ast.DOTS(l) | Ast.CIRCLES(l) | Ast.STARS(l) ->
      List.fold_left Common.union_set [] (List.map fn l)

let metaid (x,_) = x

(* Note that we would really rather attach + code to - or context code that
shares the same variables, if there is such.  If we attach it to something
else, the we increase the scope of the variable, and may allow less
variation.  Perhaps this is never a problem, because multiple control-flow
paths are only possible when there are dots, and + code can't attach to
dots.  If there are two options for attaching the + code, then both options
necessarily occur the same number of times in the matched code, so it
doesn't matter where the quantifier goes. *)

let rec mcode (_,mcodekind) =
  let process_anything_list_list anythings =
    List.fold_left Common.union_set []
      (List.map
	 (function l ->
	   List.fold_left Common.union_set [] (List.map fvanything l))
	 anythings) in
  match mcodekind with
    Ast.MINUS(_,anythings) -> process_anything_list_list !anythings
  | Ast.CONTEXT(_,befaft) ->
      (match !befaft with
	Ast.BEFORE(ll) -> process_anything_list_list ll
      | Ast.AFTER(ll) -> process_anything_list_list ll
      | Ast.BEFOREAFTER(llb,lla) ->
	  Common.union_set
	    (process_anything_list_list lla)
	    (process_anything_list_list llb)
      | Ast.NOTHING -> [])
  | _ -> failwith "unexpected plus code"

and fvanything = function
    Ast.FullTypeTag(fullType) -> fvfullType fullType
  | Ast.BaseTypeTag(baseType) -> []
  | Ast.StructUnionTag(structUnion) -> []
  | Ast.SignTag(sign) -> []
  | Ast.IdentTag(ident) -> fvident ident
  | Ast.ExpressionTag(expression) -> fvexpr expression
  | Ast.ConstantTag(constant) -> []
  | Ast.UnaryOpTag(unaryOp) -> []
  | Ast.AssignOpTag(assignOp) -> []
  | Ast.FixOpTag(fixOp) -> []
  | Ast.BinaryOpTag(binaryOp) -> []
  | Ast.ArithOpTag(arithOp) -> []
  | Ast.LogicalOpTag(logicalOp) -> []
  | Ast.DeclarationTag(declaration) -> fvdeclaration declaration
  | Ast.ParameterTypeDefTag(ptd) -> fvparameterTypeDef ptd
  | Ast.StorageTag(storage) -> []
  | Ast.Rule_elemTag(rule_elem) -> fvrule_elem rule_elem
  | Ast.ConstVolTag(const_vol) -> []
  | Ast.Token(string) -> []
  | Ast.Code(top_level) -> fvtop_level top_level

and fvident = function
    Ast.Id(name) -> []
  | Ast.MetaId(name) -> [metaid name]
  | Ast.MetaFunc(name) -> [metaid name]
  | Ast.MetaLocalFunc(name) -> [metaid name]
  | Ast.OptIdent(id) -> fvident id
  | Ast.UniqueIdent(id) -> fvident id
  | Ast.MultiIdent(id) -> fvident id

and fvexpr = function
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
  | Ast.Cast(lp,ty,rp,exp) -> Common.union_set (fvfullType ty) (fvexpr exp)
  | Ast.MetaConst(name,_) -> [metaid name]
  | Ast.MetaErr(name) -> [metaid name]
  | Ast.MetaExpr(name,_) -> [metaid name]
  | Ast.MetaExprList(name) -> [metaid name]
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

and fvfullType = function
    Ast.Type(cv,ty) -> fvtypeC ty
  | Ast.OptType(ty) -> fvfullType ty
  | Ast.UniqueType(ty) -> fvfullType ty
  | Ast.MultiType(ty) -> fvfullType ty

and fvtypeC = function
    Ast.BaseType(ty,_) -> []
  | Ast.Pointer(ty,star) -> fvfullType ty
  | Ast.Array(ty,lb,None,rb) -> fvfullType ty
  | Ast.Array(ty,lb,Some size,rb) ->
      Common.union_set (fvfullType ty) (fvexpr size)
  | Ast.StructUnionName(name,kind) -> []
  | Ast.TypeName(name)-> []
  | Ast.MetaType(name)-> [metaid name]

and fvdeclaration = function
    Ast.Init(ty,id,eq,exp,sem) ->
      Common.union_set (fvfullType ty)
	(Common.union_set (fvident id) (fvexpr exp))
  | Ast.UnInit(ty,id,sem) -> Common.union_set (fvfullType ty) (fvident id)
  | Ast.OptDecl(decl) -> fvdeclaration decl
  | Ast.UniqueDecl(decl) -> fvdeclaration decl
  | Ast.MultiDecl(decl) -> fvdeclaration decl

and fvparameterTypeDef = function
    Ast.VoidParam(ty) -> fvfullType ty
  | Ast.Param(id,ty) -> Common.union_set (fvfullType ty) (fvident id)
  | Ast.MetaParam(name) -> [metaid name]
  | Ast.MetaParamList(name) -> [metaid name]
  | Ast.PComma(cm) -> []
  | Ast.Pdots(dots) | Ast.Pcircles(dots) -> []
  | Ast.OptParam(param) -> fvparameterTypeDef param
  | Ast.UniqueParam(param) -> fvparameterTypeDef param

and fvparameter_list x = fvdots fvparameterTypeDef x

and fvrule_elem = function
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
  | Ast.MetaStmt(name) -> [metaid name]
  | Ast.MetaStmtList(name) -> [metaid name]
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

and fvtop_level = function
    Ast.FUNCTION(rule_elem_dots) -> fvdots fvrule_elem rule_elem_dots
  | Ast.DECL(decl) -> fvdeclaration decl
  | Ast.INCLUDE(incl,fl) -> []
  | Ast.FILEINFO(oldfile,newfile) -> []
  | Ast.ERRORWORDS(explist) -> []
  | Ast.CODE(rule_elem_dots) -> fvdots fvrule_elem rule_elem_dots

let free_table =
  (Hashtbl.create(50) :
     ((Lib_engine.predicate * string Ast_ctl.modif,string) Ast_ctl.generic_ctl,string list) Hashtbl.t)

let rec free_vars x =
  let res =
    match x with
      Ast_ctl.False -> []
    | Ast_ctl.True -> []
    | Ast_ctl.Pred(Lib_engine.Match(p),_) -> fvrule_elem p
    | Ast_ctl.Pred(Lib_engine.Paren(p),_) -> [p]
    | Ast_ctl.Pred(p,_) -> []
    | Ast_ctl.Not(f) -> free_vars f
    | Ast_ctl.Exists(vars,f) -> free_vars f
    | Ast_ctl.And(f1,f2) -> Common.union_set (free_vars f1) (free_vars f2)
    | Ast_ctl.Or(f1,f2) -> Common.union_set (free_vars f1) (free_vars f2)
    | Ast_ctl.Implies(f1,f2) -> Common.union_set (free_vars f1) (free_vars f2)
    | Ast_ctl.AF(f) -> free_vars f
    | Ast_ctl.AX(f) -> free_vars f
    | Ast_ctl.AG(f) -> free_vars f
    | Ast_ctl.AU(f1,f2) -> Common.union_set (free_vars f1) (free_vars f2)
    | Ast_ctl.EF(f) -> free_vars f
    | Ast_ctl.EX(f) -> free_vars f
    | Ast_ctl.EG(f) -> free_vars f
    | Ast_ctl.EU(f1,f2) -> Common.union_set (free_vars f1) (free_vars f2) in
  Hashtbl.add free_table x res;
  res

(* --------------------------------------------------------------------- *)

let add_quants formula variables =
  List.fold_left (function prev -> function v -> Ast_ctl.Exists (v,prev))
    formula variables

let pred2exists = function
  | Ast_ctl.Pred(p,Ast_ctl.Modif(v)) as x -> Ast_ctl.Exists(v,x)
  | Ast_ctl.Pred(p,Ast_ctl.UnModif(v)) as x -> Ast_ctl.Exists(v,x)
  | Ast_ctl.Pred(p,Ast_ctl.Control) as x -> x
  | _ -> failwith "not possible"

let rec add_quantifiers quantified = function
    Ast_ctl.False -> Ast_ctl.False
  | Ast_ctl.True -> Ast_ctl.True
  | Ast_ctl.Pred(Lib_engine.Match(p),_) as x ->
      let vars = Hashtbl.find free_table x in
      let fresh =
	List.filter (function x -> not (List.mem x quantified)) vars in
      add_quants (pred2exists x) fresh
  | Ast_ctl.Pred(p,v) as x -> pred2exists x
  | Ast_ctl.Not(f) -> Ast_ctl.Not(add_quantifiers quantified f)
  | Ast_ctl.Exists(vars,f) -> Ast_ctl.Exists(vars,add_quantifiers quantified f)
  | Ast_ctl.And(f1,f2) ->
      binop f1 f2 quantified (function x -> function y -> Ast_ctl.And(x,y))
  | Ast_ctl.Or(f1,f2) ->
      Ast_ctl.Or(add_quantifiers quantified f1,add_quantifiers quantified f2)
  | Ast_ctl.Implies(f1,f2) ->
      binop f1 f2 quantified (function x -> function y -> Ast_ctl.Implies(x,y))
  | Ast_ctl.AF(f) -> Ast_ctl.AF(add_quantifiers quantified f)
  | Ast_ctl.AX(f) -> Ast_ctl.AX(add_quantifiers quantified f)
  | Ast_ctl.AG(f) -> Ast_ctl.AG(add_quantifiers quantified f)
  | Ast_ctl.AU(f1,f2) ->
      binop f1 f2 quantified (function x -> function y -> Ast_ctl.AU(x,y))
  | Ast_ctl.EF(f) -> Ast_ctl.EF(add_quantifiers quantified f)
  | Ast_ctl.EX(f) -> Ast_ctl.EX(add_quantifiers quantified f)
  | Ast_ctl.EG(f) -> Ast_ctl.EG(add_quantifiers quantified f)
  | Ast_ctl.EU(f1,f2) ->
      Ast_ctl.EU(add_quantifiers quantified f1,add_quantifiers quantified f2)

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
    Ast0_cocci.DECL(decl) -> failwith "not supported"
  | Ast0_cocci.INCLUDE(inc,s) -> failwith "not supported"
  | Ast0_cocci.FILEINFO(old_file,new_file) -> failwith "not supported"
  | Ast0_cocci.FUNCTION(stmt) ->
      let start = statement (when_statement None None stmt) None in
      let _ = free_vars start in
      add_quantifiers [] start
  | Ast0_cocci.CODE(rule_elem_dots) ->
      let start = dots statement (when_dots None None rule_elem_dots) None in
      let _ = free_vars start in
      add_quantifiers [] start
  | Ast0_cocci.ERRORWORDS(exps) -> failwith "not supported"
  | Ast0_cocci.OTHER(_) -> failwith "eliminated by top_level"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let ast0toctl l =
  ctr := 0;
  List.map top_level l
