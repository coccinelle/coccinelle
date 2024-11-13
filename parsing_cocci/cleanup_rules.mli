(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

val cleanup_rules :
    Ast_cocci.rule_with_metavars list -> string list ->
      (Ast_cocci.rule_with_metavars list * string list)
