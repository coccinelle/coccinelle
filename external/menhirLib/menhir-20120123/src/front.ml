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

(* Start where [PreFront] left off. *)

let grammar =
  PreFront.grammar

(* Perform reachability analysis. *)

let grammar =
  Reachability.trim grammar

let () =
  Time.tick "Trimming"

(* If [--depend] was specified on the command line, perform
   dependency analysis and stop. *)

let () =
  match Settings.depend with
  | Settings.OMRaw
  | Settings.OMPostprocess ->
      Infer.depend grammar (* never returns *)
  | Settings.OMNone ->
      ()

(* If [--infer] was specified on the command line, perform
   type inference and stop. *)

let grammar =
  if Settings.infer then
    let grammar = Infer.infer grammar in
    Time.tick "Inferring types for nonterminals";
    grammar
  else
    grammar

(* If [--no-inline] was specified on the command line, skip the
   inlining of non terminal definitions marked with %inline. *)

let grammar =
  if Settings.inline then begin
    let grammar, inlined = 
      NonTerminalDefinitionInlining.inline grammar
    in
    if not Settings.infer && inlined && not Settings.coq then
      Error.warning []
	"you are using the standard library and/or the %inline keyword. We\n\
	 recommend switching on --infer in order to avoid obscure type error messages.";
    Time.tick "Inlining";
    grammar
  end
  else 
    grammar

(* If [--only-preprocess] or [--only-preprocess-drop] was specified on the
   command line, print the grammar and stop. Otherwise, continue. *)

let () =
  match Settings.preprocess_mode with
  | Settings.PMOnlyPreprocess mode ->
      UnparameterizedPrinter.print mode stdout grammar;
      exit 0
  | Settings.PMNormal ->
      ()

