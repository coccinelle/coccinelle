(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)


val match_re_node :
  string list (* dropped isos *) ->
  Ast_cocci.rule_elem -> Control_flow_c.node ->
  Lib_engine.metavars_binding ->
  (Ast_cocci.rule_elem * Lib_engine.metavars_binding) list
