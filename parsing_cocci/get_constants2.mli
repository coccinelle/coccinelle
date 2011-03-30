type combine =
    And of combine list | Or of combine list | Elem of string | False | True

val get_constants :
    Ast_cocci.rule list ->
      (((Ast_cocci.meta_name list) list) list) (*negated pos vars*) ->
	  (string list option (* grep result *) *
	     string list option (* non-grep result, if any *) *
	     combine option (* raw non-grep result, if any *))
