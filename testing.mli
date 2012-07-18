(*
 * Copyright 2012, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./testing.mli"
open Common

(*****************************************************************************)
(* work with tests/ *)
(*****************************************************************************)
val testone : string (*prefix*) -> string (*test*) -> bool (*compare_expected*) -> unit
val testall : string -> bool -> unit

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

(* pad:
 * I moved the parsing_c/ subsystem testing in parsing_c/test_parsing_c.ml
 * as I need it for other projects too.
 *)

val test_parse_cocci : filename -> unit

(*****************************************************************************)
(* to be called by ocaml toplevel, to test. *)
(*****************************************************************************)

val sp_of_file :
  filename (* coccifile *)  -> filename option (* isofile *) ->
  Ast_cocci.metavar list list * Ast_cocci.rule list *
      Ast_cocci.meta_name list list list *
      Ast_cocci.meta_name list list list *
      (Ast_cocci.meta_name list list list (*used after list*) *
	 (*fresh used after list*)
	 Ast_cocci.meta_name list list list *
	 (*fresh used after list seeds*)
	 Ast_cocci.meta_name list list list) *
      Ast_cocci.meta_name list list list *
      (string list option *
	 string list option *
	 Get_constants2.combine option)

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
