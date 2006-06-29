val asttoctl :
    Ast_cocci.top_level list ->
      (Lib_engine.predicate * string Ast_ctl.modif,string,unit)
	Ast_ctl.generic_ctl list
