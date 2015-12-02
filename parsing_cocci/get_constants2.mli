(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

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
