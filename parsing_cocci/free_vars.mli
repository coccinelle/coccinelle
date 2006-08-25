type anything =
    Rule_elem        of Ast_cocci.rule_elem
  | Statement        of Ast_cocci.statement
  | StatementDots    of Ast_cocci.statement Ast_cocci.dots

val free_vars : Ast_cocci.rule_with_metavars list ->
  (Ast_cocci.rule list) * ((string list) list)
