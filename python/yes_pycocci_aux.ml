open Ast_c
open Common
open Pycaml

let call_pretty f a =
  let str = ref ([] : string list) in
  let pr_elem info = str := (Ast_c.str_of_info info) :: !str in
  let pr_sp _ = () in
  f pr_elem pr_sp a;
  String.concat " " (List.rev !str)

let exprrep = call_pretty Pretty_print_c.pp_expression_gen

let stringrep mvb = match mvb with
  Ast_c.MetaIdVal        s -> s
| Ast_c.MetaFuncVal      s -> s
| Ast_c.MetaLocalFuncVal s -> s
| Ast_c.MetaExprVal      expr -> exprrep expr
| Ast_c.MetaExprListVal  expr_list -> "TODO: <<exprlist>>"
| Ast_c.MetaTypeVal      typ -> call_pretty Pretty_print_c.pp_type_gen typ
| Ast_c.MetaInitVal      ini -> call_pretty Pretty_print_c.pp_init_gen ini
| Ast_c.MetaStmtVal      statement ->
    call_pretty Pretty_print_c.pp_statement_gen statement
| Ast_c.MetaParamVal     param ->
    call_pretty Pretty_print_c.pp_param_gen param
| Ast_c.MetaParamListVal params -> "TODO: <<paramlist>>"
| Ast_c.MetaListlenVal n -> string_of_int n
| Ast_c.MetaPosVal (pos1, pos2) ->
    let print_pos = function
	Ast_cocci.Real x -> string_of_int x
      | Ast_cocci.Virt(x,off) -> Printf.sprintf "%d+%d" x off in
    Common.sprintf ("pos(%s,%s)") (print_pos pos1) (print_pos pos2)
| Ast_c.MetaPosValList positions -> "TODO: <<postvallist>>"

