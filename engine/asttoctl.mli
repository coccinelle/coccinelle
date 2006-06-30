val asttoctl :
    Ast_cocci.top_level list ->
      (Lib_engine.predicate * string Ast_ctl.modif,string,Wrapper_ctl.info)
	Ast_ctl.generic_ctl list
