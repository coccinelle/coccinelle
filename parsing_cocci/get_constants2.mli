(* Not is never used in the result.  A bit ugly, but avoids reimplementing
some operators in get_constants2 *)
type combine =
    And of combine list | Or of combine list | Not of combine
  | Elem of string | False | True

val get_constants :
    Ast_cocci.rule list ->
      (((Ast_cocci.meta_name list) list) list) (*negated pos vars*) ->
	  (string list option (* grep result *) *
	     string list option (* non-grep result, if any *) *
	     (Str.regexp * Str.regexp list * string list)
	     option (* cocci-grep result, string list used for git *) *
	     combine option (* raw non-grep result, if any *))
