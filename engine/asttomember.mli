val asttomember : Ast_cocci.rule -> string list list ->
  ((Lib_engine.predicate * string Ast_ctl.modif) list *
     (Lib_engine.predicate * string Ast_ctl.modif) list) list
