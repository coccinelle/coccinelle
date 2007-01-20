val process :
    string list (* used after *) ->
      (string * Lib_engine.metavar_binding_kind2) list (*inherited env*)->
    (Ograph_extended.nodei * (string * Lib_engine.metavar_binding_kind2) list *
       Lib_engine.predicate) list list ->
	 (Ograph_extended.nodei *
	    (string * Lib_engine.metavar_binding_kind2) list *
	    Lib_engine.predicate) list *
	   (string * Lib_engine.metavar_binding_kind2) list
