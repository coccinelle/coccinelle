val asttomember : Ast_cocci.rule -> Ast_cocci.meta_name list list ->
  (Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif)
    list list list
