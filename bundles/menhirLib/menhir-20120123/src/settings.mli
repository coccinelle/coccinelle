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

(* This module parses the command line. *)

(* The list of file names that appear on the command line. *)

val filenames: string list

(* How to deal with the type of tokens. *)

type token_type_mode =
  | TokenTypeAndCode   (* produce the definition of the [token] type and code for the parser *)
  | TokenTypeOnly      (* produce the type definition only *)
  | CodeOnly of string (* produce the code only, by relying on an external token type *)

val token_type_mode: token_type_mode

(* Whether Pager's algorithm should be used. *)

val pager: bool

(* Whether conflicts should be explained. *)

val explain: bool

(* Whether the automaton should be dumped. *)

val dump: bool

(* Whether the automaton's construction should be explained (very verbose). *)

val follow: bool

(* Whether the grammar's dependence graph should be dumped. *)

val graph: bool

(* Whether tracing instructions should be generated. *)

val trace: bool

(* Whether error recovery should be attempted. This consists
   in discarding tokens, after the [error] token has been
   shifted, until a token that can be accepted is found. *)

val recovery: bool

(* Whether one should stop and print the grammar after joining and
   expanding the grammar. *)

type print_mode =
    | PrintNormal
    | PrintUnitActions
    | PrintUnitActionsUnitTokens

type preprocess_mode =
    | PMNormal                       (* preprocess and continue *)
    | PMOnlyPreprocess of print_mode (* preprocess, print grammar, stop *)

val preprocess_mode: preprocess_mode

(* Whether one should invoke ocamlc in order to infer types for all
   nonterminals. *)

val infer: bool

(* Whether one should inline the non terminal definitions marked
   with the %inline keyword. *)

val inline: bool

(* Whether and how one should invoke ocamldep in order to compute and
   display dependencies. *)

type ocamldep_mode =
  | OMNone        (* do not invoke ocamldep *)
  | OMRaw         (* invoke ocamldep and echo its raw output *)
  | OMPostprocess (* invoke ocamldep and postprocess its output *)

val depend: ocamldep_mode

(* Whether comments should be printed or discarded. *)

val comment: bool

(* This undocumented flag suppresses prefixing of identifiers with an
   unlikely prefix in the generated code. This increases the code's
   readability, but can cause identifiers in semantic actions to be
   captured. *)

val noprefix: bool

(* This undocumented flag causes the code to be transformed by
   [Inline]. It is on by default. *)

val code_inlining: bool

(* How [ocamlc] and [ocamldep] should be invoked. *)

val ocamlc: string
val ocamldep: string

(* How verbose we should be. *)

val logG: int (* diagnostics on the grammar *)
val logA: int (* diagnostics on the automaton *)
val logC: int (* diagnostics on the generated code *)

(* Whether tasks should be timed. *)

val timings: bool

(* The base name that should be used for the files that we create.
   This name can contain a path. *)

val base: string

(* The filename of the standard library. *)

val stdlib_filename : string

(* Whether Menhir should behave as an interpreter. *)

val interpret : bool

(* Whether the interpreter should build and display concrete syntax trees. *)

val interpret_show_cst : bool

(* Whether to use the table-based back-end ([true]) or the code-based
   back-end ([false]). *)

val table : bool

(* Whether to generate a coq description of the grammar and automaton. *)

val coq : bool

(* Whether the coq description must contain completeness proofs. *)

val coq_no_complete : bool

(* Whether the coq backend should ignore types and semantic actions. *)

val coq_no_actions : bool

(* Whether unresolved LR(1) conflicts, useless precedence declarations,
   productions that are never reduced, etc. should be treated as errors. *)

val strict: bool

