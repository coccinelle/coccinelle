val free_vars : Ast_cocci.rule_with_metavars list ->
  (Ast_cocci.rule list) *
    (((Ast_cocci.meta_name list) list) list) (*fvs of the rule*) *
    (((Ast_cocci.meta_name list) list) list) (*used after list*)

