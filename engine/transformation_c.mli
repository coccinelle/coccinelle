(* note that now we do the transformation via side effect on ast *)
val transform : 
  string (* rule name *) -> string list (* dropped isos *) ->
  Lib_engine.metavars_binding -> (* inherited bindings *)
  Lib_engine.transformation_info -> 
  Control_flow_c.cflow -> Control_flow_c.cflow (* could be unit *)
