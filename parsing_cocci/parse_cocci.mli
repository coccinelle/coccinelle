val process :
    string (* filename *) -> string option (* iso filename *) ->
      bool (* verbose? *) ->
      Ast_cocci.rule list * (Free_vars.free_table list) * ((string list) list)
	  * ((Ast_cocci.statement -> string list) list)
