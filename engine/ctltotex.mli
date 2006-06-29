val totex :
  string ->
    Ast_cocci.rule list ->
      (Lib_engine.predicate * string Ast_ctl.modif, string)
	Ast_ctl.generic_ctl list list ->
	  unit
