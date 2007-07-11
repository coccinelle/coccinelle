open Common

(* full_engine takes (coccifile, isofile) and cfiles in parameters and
 * returns a list associating to the input cfiles, and maybe header
 * files that required also to be modified, the files containing the result,
 * in general files in /tmp
 *)
val full_engine : 
  (filename * filename option) -> filename list -> 
  (filename * filename option) list


(* --------------------------------------------------------------------- *)
(* Here for testing purpose. Can be called from a toplevel for example.  *)
(* --------------------------------------------------------------------- *)

val cprogram_of_file : filename -> Parse_c.program2
val cstatement_of_string  : string -> Ast_c.statement
val cexpression_of_string : string -> Ast_c.expression

val sp_of_file :
  filename (* coccifile *)  -> filename option (* isofile *) ->
  Ast_cocci.rule list * Ast_cocci.meta_name list list list * string list

val rule_elem_of_string : string -> filename option -> Ast_cocci.rule_elem


val flows_of_ast : Ast_c.program -> Control_flow_c.cflow list
val print_flow : Control_flow_c.cflow -> unit


val ctls_of_ast :
    Ast_cocci.rule list ->
      Ast_cocci.meta_name list list list ->
	(Lib_engine.ctlcocci *
	   ((Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif) list *
	      (Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif) list))
	  list list


val one_flow  : Control_flow_c.cflow list -> Control_flow_c.cflow
val one_ctl : Lib_engine.ctlcocci list list -> Lib_engine.ctlcocci

