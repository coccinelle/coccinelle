open Common

(* full_engine cfile (coccifile, isofile) -> output in outfile *)
val full_engine : 
  filename -> (filename * filename option, Lib_engine.ctlcocci) either ->  
  filename -> unit
  


(* --------------------------------------------------------------------- *)
(* Here for testing purpose. Can be called from a toplevel for example.  *)
(* --------------------------------------------------------------------- *)

val cprogram_from_file : filename -> Parse_c.program2
val cstatement_from_string  : string -> Ast_c.statement
val cexpression_from_string : string -> Ast_c.expression

(* sp_from_file coccifile isofile *)
val sp_from_file :
    filename  -> filename option ->
    Ast_cocci.rule list * string list list list * string list
val rule_elem_from_string : string -> filename option -> Ast_cocci.rule_elem


val flows : 
 Parse_c.program2 * Parse_c.parsing_stat -> Control_flow_c.cflow list
val one_flow  : Control_flow_c.cflow list -> Control_flow_c.cflow
val print_flow : Control_flow_c.cflow -> unit


val ctls :
    Ast_cocci.rule list ->
      string list list list ->
	(Lib_engine.ctlcocci *
	   ((Lib_engine.predicate * string Ast_ctl.modif) list *
	      (Lib_engine.predicate * string Ast_ctl.modif) list))
	  list list
val one_ctl : Lib_engine.ctlcocci list list -> Lib_engine.ctlcocci

