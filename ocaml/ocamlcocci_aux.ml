(* sams as for python; perhaps this could be put somewhere else *)

open Ast_c
open Common

let caller s f a =
  let str = ref ([] : string list) in
  let pr_elem info = str := (Ast_c.str_of_info info) :: !str in
  let pr_sp _ = () in
  f ~pr_elem ~pr_space:pr_sp a;
  String.concat s (List.rev !str)

let call_pretty f a = caller " " f a
let call_pretty0 f a = caller "" f a

let exprrep = call_pretty Pretty_print_c.pp_expression_gen

let stringrep = function
  Ast_c.MetaIdVal        (s,_) -> s
| Ast_c.MetaAssignOpVal op -> call_pretty Pretty_print_c.pp_assignOp_gen op
| Ast_c.MetaBinaryOpVal op -> call_pretty Pretty_print_c.pp_binaryOp_gen op
| Ast_c.MetaFuncVal      s -> s
| Ast_c.MetaLocalFuncVal s -> s
| Ast_c.MetaExprVal      (expr,_) -> exprrep expr
| Ast_c.MetaExprListVal  expr_list ->
    call_pretty Pretty_print_c.pp_arg_list_gen expr_list
| Ast_c.MetaTypeVal      typ -> call_pretty Pretty_print_c.pp_type_gen typ
| Ast_c.MetaInitVal      ini -> call_pretty Pretty_print_c.pp_init_gen ini
| Ast_c.MetaInitListVal  ini -> call_pretty Pretty_print_c.pp_init_list_gen ini
| Ast_c.MetaDeclVal      declaration ->
    call_pretty Pretty_print_c.pp_decl_gen declaration
| Ast_c.MetaFieldVal      field ->
    call_pretty Pretty_print_c.pp_field_gen field
| Ast_c.MetaFieldListVal field ->
    call_pretty Pretty_print_c.pp_field_list_gen field
| Ast_c.MetaStmtVal      statement ->
    call_pretty Pretty_print_c.pp_statement_gen statement
| Ast_c.MetaParamVal     param ->
    call_pretty Pretty_print_c.pp_param_gen param
| Ast_c.MetaParamListVal params ->
    call_pretty Pretty_print_c.pp_param_list_gen params
| Ast_c.MetaFragListVal frags ->
    call_pretty0 Pretty_print_c.pp_string_fragment_list_gen frags
| Ast_c.MetaFmtVal fmt ->
    call_pretty0 Pretty_print_c.pp_string_format_gen fmt
| Ast_c.MetaListlenVal n -> string_of_int n
| Ast_c.MetaPosVal (pos1, pos2) ->
    let print_pos = function
	Ast_cocci.Real x -> string_of_int x
      | Ast_cocci.Virt(x,off) -> Printf.sprintf "%d+%d" x off in
    Common.sprintf ("pos(%s,%s)") (print_pos pos1) (print_pos pos2)
| Ast_c.MetaPosValList positions -> "TODO: <<postvallist>>"

