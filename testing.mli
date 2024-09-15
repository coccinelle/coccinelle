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
val cpptestall : (string -> unit) -> string -> bool -> unit
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
