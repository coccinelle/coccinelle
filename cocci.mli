val full_engine : 
  ?print_input_file:bool ->
  Common.filename -> 
  (Common.filename * Common.filename option, Lib_engine.ctlcocci) 
  Common.either ->  
  unit


(* --------------------------------------------------------------------- *)
(* Here for testing purpose. Can be called from a toplevel for example.  *)
(* --------------------------------------------------------------------- *)


val cprogram_from_file : 
    Common.filename -> Ast_c.program2 * Parse_c.parsing_stat

val cstatement_from_string  : string -> Ast_c.statement
val cexpression_from_string : string -> Ast_c.expression


val sp_from_file :
  string ->
  string option ->
  Ast_cocci.rule list * Free_vars.free_table list * string list list *
  (Ast_cocci.statement -> string list) list

val rule_elem_from_string : 
    string -> Common.filename option -> Ast_cocci.rule_elem


val flows : 
  Ast_c.program2 * Parse_c.parsing_stat ->
  (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended
  list
val one_flow :
  (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended 
  list -> 
  (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended
val print_flow : 
   (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended ->
   unit


val ctls :
  Ast_cocci.rule list ->
  Free_vars.free_table list ->
  (Ast_cocci.statement -> string list) list ->
  string list list ->
  Lib_engine.ctlcocci list list


val one_ctl : Lib_engine.ctlcocci list list -> Lib_engine.ctlcocci
