(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* 'iso is the return type of parse_iso, which currently is
(Ast_cocci.metavar list * Ast0_cocci.anything list list) list *)

(*
val do_unitary :
    (Ast0_cocci.rule * Ast_cocci.metavar list * 'iso) list ->
      (Ast0_cocci.rule * Ast_cocci.metavar list) list ->
	(Ast0_cocci.rule * Ast_cocci.metavar list * 'iso) list
*)

val do_unitary :
  Ast0_cocci.parsed_rule list -> Ast0_cocci.parsed_rule list
