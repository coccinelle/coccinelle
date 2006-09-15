open Common

val full_engine : 
  filename -> (filename * filename option, Lib_engine.ctlcocci) either ->  
  unit


(* --------------------------------------------------------------------- *)
(* Here for testing purpose. Can be called from a toplevel for example.  *)
(* --------------------------------------------------------------------- *)

val cprogram_from_file : filename -> Ast_c.program2 * Parse_c.parsing_stat
val cstatement_from_string  : string -> Ast_c.statement
val cexpression_from_string : string -> Ast_c.expression


val sp_from_file :
  string -> string option ->
    Ast_cocci.rule list * string list list list * string list
val rule_elem_from_string : string -> filename option -> Ast_cocci.rule_elem


val flows : Ast_c.program2 * Parse_c.parsing_stat -> Control_flow_c.cflow list
val one_flow  : Control_flow_c.cflow list -> Control_flow_c.cflow
val print_flow : Control_flow_c.cflow -> unit


val ctls :
    Ast_cocci.rule list ->
      string list list list -> Lib_engine.ctlcocci list list
val one_ctl : Lib_engine.ctlcocci list list -> Lib_engine.ctlcocci
