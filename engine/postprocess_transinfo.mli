val process :
    Ast_cocci.meta_name list (* used after *) ->
      (Ast_cocci.meta_name * Lib_engine.metavar_binding_kind2) list
      (*inherited env*)->
    (Ograph_extended.nodei *
       (Ast_cocci.meta_name * Lib_engine.metavar_binding_kind2) list *
       Lib_engine.predicate) list list ->
	 (int list (*index*) *
	    (Ograph_extended.nodei *
	       (Ast_cocci.meta_name * Lib_engine.metavar_binding_kind2)
	       list *
	       Lib_engine.predicate)) list *
	   (Ast_cocci.meta_name * Lib_engine.metavar_binding_kind2) list list
