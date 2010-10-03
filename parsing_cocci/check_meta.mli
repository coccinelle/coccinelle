val check_meta :
    string ->
    Ast_cocci.metavar list (* old metavariables *) ->
      Ast_cocci.metavar list (* explicitly inherited *) ->
	Ast_cocci.metavar list (* declared locally *) ->
	  Ast0_cocci.rule -> Ast0_cocci.rule -> unit
