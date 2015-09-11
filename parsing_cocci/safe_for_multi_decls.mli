(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val safe_for_multi_decls :
    Ast_cocci.rule_with_metavars list ->
      Ast_cocci.rule_with_metavars list
