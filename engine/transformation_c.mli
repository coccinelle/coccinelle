(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* note that now we do the transformation via side effect on ast *)
val transform :
  string (* rule name *) -> string list (* dropped isos *) ->
  Lib_engine.metavars_binding -> (* inherited bindings *)
  Lib_engine.numbered_transformation_info ->
  Control_flow_c.cflow -> Control_flow_c.cflow (* could be unit *)
