(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Common

(*****************************************************************************)
(* work with tests/ *)
(*****************************************************************************)
val testone : string (*prefix*) -> string (*test*) ->
  string option (*compare_expected*) -> unit
val testall : (string -> unit) -> string -> bool -> unit
val test_spacing : (string -> unit) -> string -> bool -> unit

(*****************************************************************************)
(* works with tests-big/. The .res, .ok, .spatch_ok, .failed, .var *)
(*****************************************************************************)
val test_okfailed : filename (*cocci*) -> filename (*c*) list -> unit
val test_regression_okfailed : unit -> unit



(*****************************************************************************)
(* the parameter is the result of Cocci.full_engine *)
(*****************************************************************************)
val compare_with_expected : (filename * filename option) list -> string -> unit


(*****************************************************************************)
(* to test/debug the coccinelle subsystems *)
(*****************************************************************************)

(* pad:
 * I moved the parsing_c/ subsystem testing in parsing_c/test_parsing_c.ml
 * as I need it for other projects too.
 *)

val test_parse_cocci : filename -> unit
val test_rule_dependencies : filename -> unit

(*****************************************************************************)
(* to be called by ocaml toplevel, to test. *)
(*****************************************************************************)

val sp_of_file :
  filename (* coccifile *)  -> filename option (* isofile *) ->
  Ast_cocci.metavar list list * Ast_cocci.rule list * Ast_cocci.rule list *
      Ast_cocci.meta_name list list list *
      (Ast_cocci.meta_name list * Ast_cocci.meta_name list) list list *
      (Ast_cocci.meta_name list list list (*used after list*) *
	 (*fresh used after list*)
	 Ast_cocci.meta_name list list list *
	 (*fresh used after list seeds*)
	 Ast_cocci.meta_name list list list) *
      Ast_cocci.meta_name list list list *
      (string list option *
	 string list option *
	 (Str.regexp * Str.regexp list * string list) option *
	 Get_constants2.combine option) *
      bool (* format info needed for strings *) *
      bool (* contains modif in any rule *)

(* TODO: Remove
val rule_elem_of_string : string -> filename option -> Ast_cocci.rule_elem
*)

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
