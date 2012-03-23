(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

(* This module helps report errors and maintains some information
   about the source file that is being read. *)

(* ---------------------------------------------------------------------------- *)

(* Call [set_filename] before lexing and parsing in order to inform
   the module [Error] about the name of the file that is being
   examined. *)

(* TEMPORARY limiter ou supprimer ou commenter cette interface stateful *)

val set_filename: string -> unit

val get_filename: unit -> string

val get_filemark: unit -> Mark.t

val file_contents: string option ref

val get_file_contents: unit -> string

(* ---------------------------------------------------------------------------- *)

(* Logging and log levels. *)

val logG: int -> (out_channel -> unit) -> unit
val logA: int -> (out_channel -> unit) -> unit
val logC: int -> (out_channel -> unit) -> unit

(* ---------------------------------------------------------------------------- *)

(* Errors and warnings. *)

(* [error ps msg] displays the error message [msg], referring to the
   positions [ps], and exits. *)

val error: Positions.positions -> string -> 'a

(* [errorp v msg] displays the error message [msg], referring to the
   position range carried by [v], and exits. *)

val errorp: 'a Positions.located -> string -> 'b

(* [warning ps msg] displays the warning message [msg], referring to
   the positions [ps]. *)

val warning: Positions.positions -> string -> unit

(* [signal ps msg] displays the error message [msg], referring to the
   positions [ps], and does not exit immediately. *)

val signal: Positions.positions -> string -> unit

(* [errors] returns [true] if [signal] was previously called. Together
   [signal] and [errors] allow reporting multiple errors before aborting. *)

val errors: unit -> bool

(* Certain warnings about the grammar can optionally be treated as errors.
   The following function emits a warning or error message, via [warning] or
   [signal]. It does not stop the program; the client must at some point call
   [errors] and stop the program if any errors have been reported. *)

val grammar_warning: Positions.positions -> string -> unit

