val get_constants :
    Ast_cocci.rule list ->
      (((Ast_cocci.meta_name list) list) list) (*negated pos vars*) ->
	((string * bool) list (* virtual rule info *)) ->
	  string list option
