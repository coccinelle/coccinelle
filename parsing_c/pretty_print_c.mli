
type pr_elem_func = Ast_c.info -> unit

val pp_expression_gen : pr_elem_func -> Ast_c.expression -> unit
val pp_statement_gen : pr_elem_func -> Ast_c.statement -> unit
val pp_decl_gen : pr_elem_func -> Ast_c.declaration -> unit
val pp_init_gen : pr_elem_func -> Ast_c.initialiser -> unit

val pp_cst_gen : 
  pr_elem_func -> (Ast_c.constant, string) Common.either Ast_c.wrap -> unit

val pp_type_gen : pr_elem_func -> Ast_c.fullType -> unit
val pp_type_with_ident_gen :
  pr_elem_func -> 
  (string * Ast_c.info) option ->
  (Ast_c.storage * Ast_c.il) option -> Ast_c.fullType -> unit


val pp_program_gen : pr_elem_func -> Ast_c.toplevel -> unit

val pp_expression_simple : Ast_c.expression -> unit
val pp_statement_simple : Ast_c.statement -> unit


