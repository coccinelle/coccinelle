
val transform : 
  string (* rule name *) -> string list (* dropped isos *) ->
  Lib_engine.transformation_info -> 
  Control_flow_c.cflow -> Control_flow_c.cflow
