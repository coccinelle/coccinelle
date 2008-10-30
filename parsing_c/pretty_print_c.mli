
type pr_elem_func = Ast_c.info -> unit
type pr_space_func = unit -> unit

val pp_expression_gen :
    pr_elem_func -> pr_space_func -> Ast_c.expression -> unit
val pp_statement_gen : pr_elem_func -> pr_space_func -> Ast_c.statement -> unit
val pp_decl_gen : pr_elem_func -> pr_space_func -> Ast_c.declaration -> unit
val pp_init_gen : pr_elem_func -> pr_space_func -> Ast_c.initialiser -> unit
val pp_param_gen : pr_elem_func -> pr_space_func -> Ast_c.parameterType -> unit

val pp_type_gen : pr_elem_func -> pr_space_func -> Ast_c.fullType -> unit
val pp_type_with_ident_gen :
  pr_elem_func -> pr_space_func ->
  (string * Ast_c.info) option ->
  (Ast_c.storage * Ast_c.il) option -> Ast_c.fullType -> Ast_c.attribute list ->
  unit


val pp_program_gen : pr_elem_func -> pr_space_func -> Ast_c.toplevel -> unit

val pp_expression_simple : Ast_c.expression -> unit
val pp_statement_simple : Ast_c.statement -> unit
val pp_type_simple : Ast_c.fullType -> unit
val pp_toplevel_simple : Ast_c.toplevel -> unit
val pp_flow_simple: Control_flow_c.node -> unit
