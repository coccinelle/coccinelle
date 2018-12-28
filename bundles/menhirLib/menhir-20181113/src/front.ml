(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* The front-end. This module performs a series of toplevel side effects. *)

(* ------------------------------------------------------------------------- *)

(* Reading a grammar from a file. *)

let load_partial_grammar filename : Syntax.partial_grammar =
  let validExt = if Settings.coq then ".vy" else ".mly" in
  if not (Filename.check_suffix filename validExt) then
    Error.error []
      "argument file names should end in %s. \"%s\" is not accepted."
      validExt filename;
  InputFile.new_input_file filename;
  try

    let contents = IO.read_whole_file filename in
    InputFile.with_file_contents contents (fun () ->
      let open Lexing in
      let lexbuf = Lexing.from_string contents in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      (* the grammar: *)
      { (Driver.grammar Lexer.main lexbuf)
        with Syntax.pg_filename = filename }
    )

  with Sys_error msg ->
    Error.error [] "%s" msg

(* ------------------------------------------------------------------------- *)

(* Read all of the grammar files that are named on the command line. *)

let grammars : Syntax.partial_grammar list =
  List.map load_partial_grammar Settings.filenames

let () =
  Time.tick "Lexing and parsing"

(* ------------------------------------------------------------------------- *)

(* Eliminate anonymous rules. *)

let grammars : Syntax.partial_grammar list =
  List.map Anonymous.transform_partial_grammar grammars

(* ------------------------------------------------------------------------- *)

(* If several grammar files were specified, merge them. *)

let grammar : Syntax.grammar =
  PartialGrammar.join_partial_grammars grammars

(* ------------------------------------------------------------------------- *)

(* Check that the grammar is well-sorted; infer the sort of every symbol. *)

let sorts =
  SortInference.infer grammar

(* ------------------------------------------------------------------------- *)

(* Expand away all applications of parameterized nonterminal symbols, so as
   to obtain a grammar without parameterized nonterminal symbols. *)

let grammar : UnparameterizedSyntax.grammar =
  let module S = SelectiveExpansion in
  (* First, perform a selective expansion: expand away all parameters of
     higher sort, keeping the parameters of sort [*]. This process always
     terminates. *)
  let grammar1 = S.expand S.ExpandHigherSort sorts grammar in
  (* This "first-order parameterized grammar" can then be submitted to
     the termination check. *)
  CheckSafeParameterizedGrammar.check grammar1;
  (* If it passes the check, then full expansion is safe. We drop [grammar1]
     and start over from [grammar]. This is required in order to get correct
     names. (Expanding [grammar1] would yield an equivalent grammar, with
     more complicated names, reflecting the two steps of expansion.) *)
  let grammar = S.expand S.ExpandAll sorts grammar in
  (* This yields an unparameterized grammar. *)
  Drop.drop grammar

let () =
  Time.tick "Joining and expanding"

(* ------------------------------------------------------------------------- *)

(* If [--only-tokens] was specified on the command line, produce
   the definition of the [token] type and stop. *)

let () =
  TokenType.produce_tokentypes grammar

(* ------------------------------------------------------------------------- *)

(* Perform reachability analysis. *)

let grammar =
  Reachability.trim grammar

let () =
  Time.tick "Trimming"

(* ------------------------------------------------------------------------- *)

(* If [--infer] was specified on the command line, perform type inference.
   The OCaml type of every nonterminal symbol is then known. *)

(* If [--depend] or [--raw-depend] was specified on the command line,
   perform dependency analysis and stop. *)

(* The purpose of [--depend] and [--raw-depend] is to support [--infer].
   Indeed, [--infer] is implemented by producing a mock [.ml] file (which
   contains just the semantic actions) and invoking [ocamlc]. This requires
   certain [.cmi] files to exist. So, [--(raw-)depend] is a way for us to
   announce which [.cmi] files we need. It is implemented by producing the
   mock [.ml] file and running [ocamldep] on it. We also produce a mock
   [.mli] file, even though in principle it should be unnecessary -- see
   comment in [nonterminalType.mli]. *)

(* If [--infer-write-query] was specified on the command line, write a
   mock [.ml] file and stop. It is then up to the user (or build system)
   to invoke [ocamlc -i] on this file, so as to do type inference. *)

(* If [--infer-read-reply] was specified on the command line, read the
   inferred [.mli] file. The OCaml type of every nonterminal symbol is
   then known, just as with [--infer]. *)

let grammar, ocaml_types_have_been_checked =
  Settings.(match infer with
  | IMNone ->
      grammar, false
  | IMInfer ->
      let grammar = Infer.infer grammar in
      Time.tick "Inferring types for nonterminals";
      grammar, true
  | IMDependRaw ->
      Infer.depend false grammar         (* never returns *)
  | IMDependPostprocess ->
      Infer.depend true grammar          (* never returns *)
  | IMWriteQuery filename ->
      Infer.write_query filename grammar (* never returns *)
  | IMReadReply filename ->
      let grammar = Infer.read_reply filename grammar in
      Time.tick "Reading inferred types for nonterminals";
      grammar, true
  )

(* ------------------------------------------------------------------------- *)

(* Expand away some of the position keywords. *)

let grammar =
  KeywordExpansion.expand_grammar grammar

(* ------------------------------------------------------------------------- *)

(* If [--no-inline] was specified on the command line, skip the
   inlining of non terminal definitions marked with %inline. *)

let grammar =
  if Settings.inline then begin
    let grammar = NonTerminalDefinitionInlining.inline grammar in
    (* 2018/05/23 Removed the warning that was issued when %inline was used
       but --infer was turned off. Most people should use ocamlbuild or dune
       anyway. *)
    Time.tick "Inlining";
    grammar
  end
  else
    grammar

(* ------------------------------------------------------------------------- *)

(* If [--only-preprocess] or [--only-preprocess-drop] was specified on the
   command line, print the grammar and stop. Otherwise, continue. *)

let () =
  match Settings.preprocess_mode with
  | Settings.PMOnlyPreprocess mode ->
      UnparameterizedPrinter.print mode stdout grammar;
      exit 0
  | Settings.PMNormal ->
      ()
