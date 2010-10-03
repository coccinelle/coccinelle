
type pr_elem_func = Ast_c.info -> unit
type pr_space_func = unit -> unit
type pr_nl_func = unit -> unit
type pr_indent_func = unit -> unit
type pr_outdent_func = unit -> unit
type pr_unindent_func = unit -> unit

type expression_printer = Ast_c.expression -> unit
type arg_list_printer = Ast_c.argument Ast_c.wrap2 list -> unit
type statement_printer = Ast_c.statement -> unit
type declaration_printer = Ast_c.declaration -> unit
type initialiser_printer = Ast_c.initialiser -> unit
type param_printer = Ast_c.parameterType -> unit
type type_printer = Ast_c.fullType -> unit
type type_with_ident_printer =
    (string * Ast_c.info) option ->
      (Ast_c.storage * Ast_c.il) option -> Ast_c.fullType ->
	Ast_c.attribute list -> unit
type toplevel_printer = Ast_c.toplevel -> unit
type flow_printer = Control_flow_c.node -> unit

type pretty_printers =
    {expression : expression_printer;
      arg_list : arg_list_printer;
      statement : statement_printer;
      decl : declaration_printer;
      init : initialiser_printer;
      param : param_printer;
      ty : type_printer;
      type_with_ident : type_with_ident_printer;
      toplevel : toplevel_printer;
      flow : flow_printer}

val pretty_print_c :
    pr_elem_func -> pr_space_func -> pr_nl_func -> pr_indent_func ->
      pr_outdent_func -> pr_unindent_func -> pretty_printers


val pp_expression_gen : pr_elem_func -> pr_space_func -> expression_printer
val pp_arg_list_gen : pr_elem_func -> pr_space_func -> arg_list_printer
val pp_statement_gen : pr_elem_func -> pr_space_func -> statement_printer
val pp_decl_gen : pr_elem_func -> pr_space_func -> declaration_printer
val pp_init_gen : pr_elem_func -> pr_space_func -> initialiser_printer
val pp_param_gen : pr_elem_func -> pr_space_func -> param_printer

val pp_type_gen : pr_elem_func -> pr_space_func -> type_printer
val pp_type_with_ident_gen :
    pr_elem_func -> pr_space_func -> type_with_ident_printer

val pp_program_gen : pr_elem_func -> pr_space_func -> toplevel_printer


val pp_expression_simple : expression_printer
val pp_statement_simple : statement_printer
val pp_type_simple : type_printer
val pp_init_simple : initialiser_printer
val pp_toplevel_simple : toplevel_printer
val pp_flow_simple: flow_printer


val debug_info_of_node: Ograph_extended.nodei -> Control_flow_c.cflow -> string

val string_of_expression: Ast_c.expression -> string
