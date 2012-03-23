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

open Printf
open Lexing

(* TEMPORARY Vérifier que les messages d'erreur sont standardisés au
   maximum, localisés au maximum. Supprimer autant de fonctions que
   possible dans ce module. *)

(* TEMPORARY reprendre compl`etement implementation et interface
   de ce module *)

(* ---------------------------------------------------------------------------- *)

(* Global state. *)

let get_initialized_ref ref =
  match !ref with
  | None ->
      assert false
  | Some contents ->
      contents

let filename =
  ref (None : string option)

let filemark =
  ref Mark.none

(* 2011/10/19: do not use [Filename.basename]. The [#] annotations that
   we insert in the [.ml] file must retain their full path. This does
   mean that the [#] annotations depend on how menhir is invoked -- e.g.
   [menhir foo/bar.mly] and [cd foo && menhir bar.mly] will produce
   different files. Nevertheless, this seems useful/reasonable. *)

(* This also influences the type error messages produced by [--infer]. *)

let set_filename name =
  filename := Some name;
  filemark := Mark.fresh()

let get_filename () =
  get_initialized_ref filename

let get_filemark () =
  !filemark

let file_contents =
  ref (None : string option)

let get_file_contents () =
  get_initialized_ref file_contents

(* ---------------------------------------------------------------------------- *)

(* Logging and log levels. *)

let log kind verbosity msg =
  if kind >= verbosity then
    Printf.fprintf stderr "%t%!" msg

let logG =
  log Settings.logG

let logA =
  log Settings.logA

let logC =
  log Settings.logC

(* ---------------------------------------------------------------------------- *)

(* Errors and warnings. *)

let errors =
  ref false

let printN positions message = 
  List.iter (fun position -> 
    fprintf stderr "%s:\n" (Positions.string_of_pos position)
  ) positions;
  fprintf stderr "%s\n%!" message

let error_message message =
  "Error: " ^ message

let error positions message =
  printN positions (error_message message);
  exit 1

let errorp v message =
  error [ Positions.position v ] message

let signal positions message =
  printN positions message;
  errors := true

let warning positions message =
  printN positions (Printf.sprintf "Warning: %s" message)

let errors () =
  !errors

(* Certain warnings about the grammar can optionally be treated as errors.
   The following function emits a warning or error message, via [warning] or
   [signal]. It does not stop the program; the client must at some point call
   [errors] and stop the program if any errors have been reported. *)

let grammar_warning positions message =
  if Settings.strict then
    signal positions (error_message message)
  else
    warning positions message

