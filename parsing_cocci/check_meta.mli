(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val check_meta :
    string ->
    Ast_cocci.metavar list (* old metavariables *) ->
      Ast_cocci.metavar list (* explicitly inherited *) ->
	Ast_cocci.metavar list (* declared locally *) ->
	  Ast0_cocci.rule -> Ast0_cocci.rule -> unit
