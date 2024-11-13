(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

type cocci_predicate = Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif
type top_formula = NONDECL of Lib_engine.ctlcocci | CODE of Lib_engine.ctlcocci

val asttoctl :
    Ast_cocci.rule ->
      (Ast_cocci.meta_name list list (* used after *) *
	 Ast_cocci.meta_name list list (* fresh used after *) *
	 Ast_cocci.meta_name list list (* fresh used after seeds *)) ->
      Ast_cocci.meta_name list list (* positions *) ->
      top_formula list
