val totex :
  string ->
    Ast_cocci.rule list ->
      ((Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif,
	Ast_cocci.meta_name,Wrapper_ctl.info)
	 Ast_ctl.generic_ctl * 'a) list list ->
	   unit
