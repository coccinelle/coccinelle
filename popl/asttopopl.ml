module Ast = Ast_cocci
module Past = Ast_popl

(* --------------------------------------------------------------------- *)

let rec stm s =
  match Ast.unwrap s with
    Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.ExprStatement(_,_) -> Past.Term ast
      |	_ -> failwith "complex statements not supported")
  | Ast.Disj([sd1;sd2]) ->
      Past.Or (stm_list sd1,stm_list sd2)
  | Ast.Dots(dots,whencodes,_) ->
      (match whencodes with
	Ast.NoWhen -> Past.DInfo(Past.Dots,[],[])
      |	Ast.WhenNot(a) -> Past.DInfo(Past.When(Past.Dots,stm_list a),[],[])
      |	_ -> failwith "only when != supported")
  | Ast.Nest(stmt_dots,whencodes,_) ->
      let nest = Past.Nest(stm_list stmt_dots) in
      (match whencodes with
	Ast.NoWhen -> Past.DInfo(nest,[],[])
      |	Ast.WhenNot(a) -> Past.DInfo(Past.When(nest,stm_list a),[],[])
      |	_ -> failwith "only when != supported")
  | _ -> failwith "unsupported statement"

and stm_list s =
  match Ast.unwrap s with
    Ast.DOTS(d) ->
      List.fold_right
	(function cur -> function rest -> Past.Seq(stm cur, rest))
	d Past.Empty
  | _ -> failwith "only DOTS handled"

let top s =
  match Ast.unwrap s with
    Ast.CODE(stmt_dots) -> stm_list stmt_dots
  | _ -> failwith "only CODE handled"
