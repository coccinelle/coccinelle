(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val process : string (* name *) -> Ast_cocci.metavar list ->
  string list (* dropped isos *) ->
    Ast0_cocci.rule -> Ast0_cocci.rule -> Ast_cocci.ruletype ->
      ((Ast_cocci.metavar list * Ast0_cocci.rule) *
	 Ast_cocci.rule_with_metavars option)
