module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module CTL = Ast_ctl

let warning s = Printf.fprintf stderr "warning: %s\n" s

(* --------------------------------------------------------------------- *)

type predicate =
    State of string
  | TrueBranch | FalseBranch | After
  | Paren of string
  | Match of Ast.rule_elem

let pred2c = function
    State(s) -> "\\msf{State}("^s^")"
  | TrueBranch -> "\\msf{TrueBranch}"
  | FalseBranch -> "\\msf{FalseBranch}"
  | After -> "\\msf{After}"
  | Paren(s) -> "\\msf{Paren}("^s^")"
  | Match(re) -> Unparse_cocci.rule_elem_to_string re

(* --------------------------------------------------------------------- *)

let get_option fn = function
    None -> None
  | Some x -> Some (fn x)

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

let make_seq first rest =
  let template f =
    let sv = fresh_var() in
    CTL.Exists(sv,f(CTL.And(CTL.Pred(State sv),first))) in
  match rest with
    None -> template (function x -> x)
  | Some rest -> template (function x -> CTL.And(x,CTL.AX(rest)))

let rec statement stmt after =
  match stmt with
    Ast0.Decl(decl) ->
      make_seq (CTL.Pred(Match(Ast.Decl(Ast0toast.declaration decl)))) after
  | Ast0.Seq(lbrace,body,rbrace) ->
      let v = fresh_var() in
      let start_brace =
	CTL.And(CTL.Pred(Match(Ast.SeqStart(Ast0toast.mcode lbrace))),
		CTL.Pred(Paren v)) in
      let end_brace =
	CTL.And
	  (CTL.Pred(Match(Ast.SeqEnd(Ast0toast.mcode rbrace))),
	   CTL.Pred(Paren v)) in
      make_seq start_brace
	(Some(dots statement body (Some (make_seq end_brace after))))
  | Ast0.ExprStatement(exp,sem) ->
      make_seq
	(CTL.Pred
	   (Match
	      (Ast.ExprStatement
		 (Ast0toast.expression exp,Ast0toast.mcode sem))))
	after
  | Ast0.IfThen(iff,lp,exp,rp,branch) ->
      let if_header =
	CTL.Pred
	  (Match
	     (Ast.IfHeader
		(Ast0toast.mcode iff,Ast0toast.mcode lp,
		  Ast0toast.expression exp,Ast0toast.mcode rp))) in
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
	CTL.Pred
	  (Match
	     (Ast.IfHeader
		(Ast0toast.mcode iff,Ast0toast.mcode lp,
		  Ast0toast.expression exp,Ast0toast.mcode rp))) in
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
	CTL.Pred
	  (Match
	     (Ast.WhileHeader
		(Ast0toast.mcode wh,Ast0toast.mcode lp,
		 Ast0toast.expression exp,Ast0toast.mcode rp))) in
      let body_line = CTL.Implies(CTL.Pred(TrueBranch),statement body after) in
      make_seq while_header (Some body_line)
  | Ast0.Return(ret,sem) ->
      make_seq
	(CTL.Pred(Match(Ast.Return(Ast0toast.mcode ret,Ast0toast.mcode sem))))
	after
  | Ast0.ReturnExpr(ret,exp,sem) ->
      let return = 
	Ast.ReturnExpr
	  (Ast0toast.mcode ret,Ast0toast.expression exp,Ast0toast.mcode sem) in
      make_seq (CTL.Pred (Match return)) after
  | Ast0.MetaStmt(name) ->
      make_seq (CTL.Pred(Match(Ast.MetaStmt(Ast0toast.mcode name)))) after
  | Ast0.MetaStmtList(name) ->
      make_seq (CTL.Pred(Match(Ast.MetaStmtList(Ast0toast.mcode name)))) after
  | Ast0.Exp(exp) ->
      make_seq (CTL.Pred(Match(Ast.Exp(Ast0toast.expression exp)))) after
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
	CTL.Pred(Match(Ast.FunDecl(stg,name,lp,params,rp))) in
      let v = fresh_var() in
      let start_brace =
	CTL.And(CTL.Pred(Match(Ast.SeqStart(Ast0toast.mcode lbrace))),
		CTL.Pred(Paren v)) in
      let end_brace =
	CTL.And
	  (CTL.Pred(Match(Ast.SeqEnd(Ast0toast.mcode rbrace))),
	   CTL.Pred(Paren v)) in
      make_seq function_header
	(Some
	   (make_seq start_brace
	      (Some(dots statement body (Some(make_seq end_brace after))))))
  | Ast0.OptStm(stm) ->
      warning "arities not yet supported"; statement stm after
  | Ast0.UniqueStm(stm) ->
      warning "arities not yet supported"; statement stm after
  | Ast0.MultiStm(stm) ->
      warning "arities not yet supported"; statement stm after
  | _ -> failwith "not supported"

let rec add_quantifiers quantified = function
    CTL.False -> CTL.False
  | CTL.True -> CTL.True
  | CTL.Pred(Match(p)) ->  of 'a
  | CTL.Pred(p) -> CTL.Pred(p)
  | CTL.Not(f) -> CTL.Not(add_quantifiers quantified f)
  | CTL.Exists(vars,f) -> CTL.Exists(vars,add_quantifiers (vars@quantified) f)
  | CTL.And(f1,f2) ->
  | CTL.Or(f1,f2) ->
  | CTL.Implies(f1,f2) ->
  | CTL.AF(f) -> CTL.AF(add_quantifiers quantified f)
  | CTL.AX(f) -> CTL.AX(add_quantifiers quantified f)
  | CTL.AG(f) -> CTL.AG(add_quantifiers quantified f)
  | CTL.AU(f1,f2) ->
      CTL.AU(add_quantifiers quantified f1,add_quantifiers quantified f2)
  | CTL.EF(f) -> CTL.EF(add_quantifiers quantified f)
  | CTL.EX(f) -> CTL.EX(add_quantifiers quantified f)
  | CTL.EG(f) -> CTL.EG(add_quantifiers quantified f)
  | CTL.EU(f1,f2) ->
      CTL.EU(add_quantifiers quantified f1,add_quantifiers quantified f2)
	
(* --------------------------------------------------------------------- *)
(* Function declaration *)
(* Haven't thought much about arity here... *)

let top_level = function
    Ast0.DECL(decl) -> failwith "not supported"
  | Ast0.INCLUDE(inc,s) -> failwith "not supported"
  | Ast0.FILEINFO(old_file,new_file) -> failwith "not supported"
  | Ast0.FUNCTION(stmt) -> add_quantifiers [] (statement stmt None)
  | Ast0.CODE(rule_elem_dots) ->
      add_quantifiers [] (dots statement rule_elem_dots None)
  | Ast0.ERRORWORDS(exps) -> failwith "not supported"
  | Ast0.OTHER(_) -> failwith "eliminated by top_level"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let ast0toctl = List.map top_level
