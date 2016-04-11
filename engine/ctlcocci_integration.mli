(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val labels_for_ctl :
  string list (* dropped isos *) ->
  (Control_flow_c.G.key * Control_flow_c.node) list -> Lib_engine.metavars_binding ->
  Lib_engine.label_ctlcocci


val fix_flow_ctl : Control_flow_c.cflow -> Control_flow_c.cflow

val model_for_ctl :
  string list (* dropped isos *) ->
  Control_flow_c.cflow -> Lib_engine.metavars_binding -> Lib_engine.model

type pred = Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif

val mysat :
  Lib_engine.model ->
  (Lib_engine.ctlcocci * (pred list list)) ->
  (string (*rulename*) * Lib_engine.mvar list * Lib_engine.metavars_binding)->
  (Lib_engine.numbered_transformation_info *  bool *
     Lib_engine.metavars_binding * Lib_engine.metavars_binding list)


val print_bench : unit -> unit
