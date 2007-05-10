val free_vars : Ast_cocci.rule_with_metavars list ->
  string list (* all names *) * string list list (* all dependencies *) *
    (Ast_cocci.rule list) * (((Ast_cocci.meta_name list) list) list)
