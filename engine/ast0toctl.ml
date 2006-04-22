(* Arities matter for the minus slice, but not for the plus slice. *)

(* + only allowed on code in a nest (in_nest = true).  ? only allowed on
rule_elems, and on subterms if the context is ? also. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module CTL = Ast_ctl

(* --------------------------------------------------------------------- *)

type predicates =
    State of string
  | TrueBranch | FalseBranch | After
  | Paren of string
  | Match of Ast.rule_elem

(* --------------------------------------------------------------------- *)

let get_option fn = function
    None -> None
  | Some x -> Some (fn x)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

let dots fn l after =
  match l with
    Ast0.DOTS(x) -> List.fold_right fn x after
  | Ast0.CIRCLES(x) -> failwith "not supported"
  | Ast0.STARS(x) -> failwith "not supported"

(* --------------------------------------------------------------------- *)
(* Top-level code *)

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
      make_seq start_brace (dots statement l (make_seq end_brace after))
  | Ast0.ExprStatement(exp,sem) ->
      make_seq
	(CTL.Pred
	   (Match(Ast.ExprStatement(expression exp,Ast0toast.mcode sem))))
	after
  | Ast0.IfThen(iff,lp,exp,rp,branch) ->
      let if_header =
	CTL.Pred
	  (Match
	     (Ast.IfHeader
		(Ast0toast.mcode iff,Ast0toast.mcode lp,
		  Ast0toast.expression exp,Ast0toast.mcode rp))) in
      let then_line =
	CTL.Implies(CTL.Pred(TrueBranch),statement branch CTL.True) in
      let else_line = CTL.Implies(CTL.Pred(FalseBranch),CTL.False) in
      let after_line = CTL.Implies(CTL.Pred(After),after) in
      make_seq if_header (CTL.And (CTL.And then_line else_line) after_line)
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
      let if_header =
	CTL.Pred
	  (Match
	     (Ast.IfHeader
		(Ast0toast.mcode iff,Ast0toast.mcode lp,
		  Ast0toast.expression exp,Ast0toast.mcode rp))) in
      let then_line =
	CTL.Implies(CTL.Pred(TrueBranch),statement branch1 CTL.True) in
      let else_line =
	CTL.Implies(CTL.Pred(FalseBranch),statement branch2 CTL.False) in
      let after_line = CTL.Implies(CTL.Pred(After),after) in
      make_seq if_header (CTL.And (CTL.And then_line else_line) after_line)
  | Ast0.While(wh,lp,exp,rp,body) ->
      let while_header =
	CTL.Pred
	  (Match
	     (Ast.WhileHeader
		(Ast0toast.mcode wh,Ast0toast.mcode lp,
		 Ast0toast.expression exp,Ast0toast.mcode rp)) in
      let body_line = CTL.Implies(CTL.Pred(TrueBranch),statement body after) in
      make_seq while_header body_line
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
      make_seq (CTL.Pred(Ast.Exp(expression exp))) after
  | Ast0.Disj(rule_elem_dots_list) ->
      List.fold_right
	(function rest ->
	  function cur -> Ast.Or(dots statement cur CTL.True,rest))
	Ast.False rule_elem_dots_list
  | Ast0.Nest(rule_elem_dots) ->
      let dots_pattern = dots statement rule_elem_dots CTL.True in
      CTL.AU(CTL.Or(dots_pattern,CTL.Not dots_pattern),after)
  | Ast0.Dots(dots,None)    -> CTL.AF(after)
  | Ast0.Dots(dots,Some statement_dots)    ->
      CTL.AU(CTL.Not(dots statement CTL.True statement_dots),after)
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
	(make_seq start_brace (dots statement l (make_seq end_brace after)))
  | Ast0.OptStm(stm) ->
      warning "arities not yet supported"; statement stm after
  | Ast0.UniqueStm(stm) ->
      warning "arities not yet supported"; statement stm after
  | Ast0.MultiStm(stm) ->
      warning "arities not yet supported"; statement stm after
  | _ -> failwith "not supported"
	
(* --------------------------------------------------------------------- *)
(* Function declaration *)
(* Haven't thought much about arity here... *)

let top_level = function
    Ast0.DECL(decl) -> failwith "not supported"
  | Ast0.INCLUDE(inc,s) -> failwith "not supported"
  | Ast0.FILEINFO(old_file,new_file) -> failwith "not supported"
  | Ast0.FUNCTION(stmt) -> statement stmt CTL.True
  | Ast0.CODE(rule_elem_dots) -> dots statement rule_elem_dots CTL.True
  | Ast0.ERRORWORDS(exps) -> failwith "not supported"
  | Ast0.OTHER(_) -> failwith "eliminated by top_level"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let ast0toast = List.map top_level
