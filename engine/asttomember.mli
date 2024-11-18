(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

val asttomember : Ast_cocci.rule -> Ast_cocci.meta_name list list ->
  (Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif)
    list list list
