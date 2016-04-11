type type_with_ident =
    (string * Ast_c.info) option ->
    (Ast_c.storage * Ast_c.il) option ->
    Ast_c.fullType ->
    Ast_c.attribute list -> unit

type 'a printer = 'a -> unit

type pretty_printers = {
  expression      : Ast_c.expression printer;
  assignOp        : Ast_c.assignOp printer;
  binaryOp        : Ast_c.binaryOp printer;
  arg_list        : (Ast_c.argument Ast_c.wrap2 list) printer;
  arg             : Ast_c.argument printer;
  statement       : Ast_c.statement printer;
  statement_seq_list : Ast_c.statement_sequencable list printer;
  decl            : Ast_c.declaration printer;
  field           : Ast_c.field printer;
  field_list      : Ast_c.field list printer;
  init            : Ast_c.initialiser printer;
  init_list       : (Ast_c.initialiser Ast_c.wrap2 list) printer;
  param           : Ast_c.parameterType printer;
  paramlist       : (Ast_c.parameterType Ast_c.wrap2 list) printer;
  ty              : Ast_c.fullType printer;
  type_with_ident : type_with_ident;
  toplevel        : Ast_c.toplevel printer;
  fragment        : Ast_c.string_fragment printer;
  fragment_list   : (Ast_c.string_fragment list) printer;
  format          : Ast_c.string_format printer;
  flow            : Control_flow_c.node printer
}

val mk_pretty_printers :
  pr_elem:Ast_c.info printer ->
  pr_space:unit printer ->
  pr_nl: unit printer ->
  pr_indent: unit printer ->
  pr_outdent: unit printer ->
  pr_unindent: unit printer ->
  pretty_printers

(* used in pycocci mostly *)
val pp_expression_gen: pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  Ast_c.expression printer
val pp_assignOp_gen: pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  Ast_c.assignOp printer
val pp_binaryOp_gen: pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  Ast_c.binaryOp printer
val pp_arg_list_gen: pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  (Ast_c.argument Ast_c.wrap2 list) printer
val pp_arg_gen: pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  Ast_c.argument printer
val pp_decl_gen: pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  Ast_c.declaration printer
val pp_field_gen: pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  Ast_c.field printer
val pp_field_list_gen: pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  Ast_c.field list printer
val pp_statement_gen: pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  Ast_c.statement printer
val pp_statement_seq_list_gen:
    pr_elem:Ast_c.info printer -> pr_space: unit printer ->
      Ast_c.statement_sequencable list printer
val pp_param_gen:  pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  Ast_c.parameterType printer
val pp_param_list_gen:  pr_elem:Ast_c.info printer -> pr_space: unit printer ->
  (Ast_c.parameterType Ast_c.wrap2 list) printer
val pp_type_gen:  pr_elem:Ast_c.info printer -> pr_space:unit printer ->
  Ast_c.fullType printer
val pp_init_gen:  pr_elem:Ast_c.info printer -> pr_space:unit printer ->
  Ast_c.initialiser printer
val pp_init_list_gen:  pr_elem:Ast_c.info printer -> pr_space:unit printer ->
  (Ast_c.initialiser Ast_c.wrap2 list) printer
val pp_string_fragment_list_gen:
    pr_elem:Ast_c.info printer -> pr_space:unit printer ->
      Ast_c.string_fragment list printer
val pp_string_format_gen:
    pr_elem:Ast_c.info printer -> pr_space:unit printer ->
      Ast_c.string_format printer
val pp_program_gen : pr_elem:Ast_c.info printer -> pr_space:unit printer ->
  Ast_c.toplevel printer


(* used in pretty_print_engine.ml mostly *)
val pp_expression_simple: Ast_c.expression printer
val pp_assignOp_simple: Ast_c.assignOp printer
val pp_binaryOp_simple: Ast_c.binaryOp printer
val pp_init_simple:       Ast_c.initialiser printer
val pp_type_simple:       Ast_c.fullType printer
val pp_decl_simple:       Ast_c.declaration printer
val pp_field_simple:      Ast_c.field printer
val pp_statement_simple:  Ast_c.statement printer
val pp_statement_seq_list_simple: Ast_c.statement_sequencable list printer
val pp_toplevel_simple:   Ast_c.toplevel printer
val pp_string_fragment_simple:   Ast_c.string_fragment printer
val pp_string_format_simple:     Ast_c.string_format printer

val debug_info_of_node:
  Control_flow_c.G.key -> Control_flow_c.cflow -> string

val string_of_expression: Ast_c.expression -> string
(* Normalized string representation of an [Ifdef] guard.
 *
 * Ignored #if conditions (cf. [Gnone]) are treated as 0, which is consistent
 * with the way Coccinelle handles them.
 *
 * @author Iago Abal
 *)
val string_of_ifdef_guard: Ast_c.ifdef_guard -> string
val string_of_toplevel: Ast_c.toplevel -> string
