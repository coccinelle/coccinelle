(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

exception Bad_virt of string

val parse : string ->
    (string, string) Common.either Common.set (* iso files *) *
    Ast0_cocci.parsed_rule list (* rules *) *
    string list (* virtuals *) *
    Ast_cocci.metavar list (* metavariables *)

val process :
    string (* filename *) -> string option (* iso filename *) ->
      bool (* verbose? *) ->
	(Ast_cocci.metavar list list) * (Ast_cocci.rule list) *
	  Ast_cocci.meta_name list list list (*fvs of the rule*) *
	  Ast_cocci.meta_name list list list (*negated pos vars*) *
	  (Ast_cocci.meta_name list list list (*used after list*) *
	     (*fresh used after list*)
	     Ast_cocci.meta_name list list list *
	     (*fresh used after list seeds*)
	     Ast_cocci.meta_name list list list) *
	  Ast_cocci.meta_name list list list (*positions list*) *
	  (string list option (*non metavars in - code, for grep*) *
	     string list option (*non metavars in - code, for glimpse/google*)*
	     (Str.regexp * Str.regexp list * string list)
	     option(*cocci-grep/git grep result, if any*)
	     * (*non metavars in - code, for other tools*)
	     Get_constants2.combine option) *
	     (* true if string constants need to be parsed *)
	  bool
