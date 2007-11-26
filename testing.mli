open Common

(*****************************************************************************)
(* work with tests/ *)
(*****************************************************************************)
val testone : string (*test*) -> bool (*compare_expected*) -> unit
val testall : unit -> unit

(*****************************************************************************)
(* works with tests-big/. The .res, .ok, .spatch_ok, .failed, .var *)
(*****************************************************************************)
val test_okfailed : filename (*cocci*) -> filename (*c*) list -> unit
val test_regression_okfailed : unit -> unit



(*****************************************************************************)
(* the parameter is the result of Cocci.full_engine *)
(*****************************************************************************)
val compare_with_expected : (filename * filename option) list -> unit 


(*****************************************************************************)
(* to test/debug the coccinelle subsystems *)
(*****************************************************************************)

val test_tokens_c : filename -> unit
(* parse and handle some regression information when called with dirmode *)
val test_parse_c  : filename list -> bool (* dirmode *) -> unit
val test_parse_h  : filename list -> bool (* dirmode *) -> unit
val test_parse_ch : filename list -> bool (* dirmode *) -> unit

val test_parse_unparse : filename -> unit

val test_cfg : filename (* foo.c or foo.c:main *) -> unit
val test_type_c : filename -> unit

val test_compare_c : filename -> filename -> unit (* result is in unix code *)
val test_compare_c_hardcoded : unit -> unit

val test_parse_cocci : filename -> unit

val test_xxx : string list -> bool (* dirmode *) -> unit

(*****************************************************************************)
(* to be called by ocaml toplevel, to test. *)
(*****************************************************************************)

val cprogram_of_file : filename -> Parse_c.program2
val cstatement_of_string  : string -> Ast_c.statement
val cexpression_of_string : string -> Ast_c.expression

val sp_of_file :
  filename (* coccifile *)  -> filename option (* isofile *) ->
  Ast_cocci.rule list * Ast_cocci.meta_name list list list *
      Ast_cocci.meta_name list list list * Ast_cocci.meta_name list list list *
      string list list
val rule_elem_of_string : string -> filename option -> Ast_cocci.rule_elem


(*
val flows_of_ast : Ast_c.program -> Control_flow_c.cflow list
val print_flow : Control_flow_c.cflow -> unit

val ctls_of_ast :
    Ast_cocci.rule list ->
      Ast_cocci.meta_name list list list ->
	(Lib_engine.ctlcocci *
	   ((Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif)
	      list list))
	  list list


val one_flow  : Control_flow_c.cflow list -> Control_flow_c.cflow
val one_ctl : Lib_engine.ctlcocci list list -> Lib_engine.ctlcocci
*)
