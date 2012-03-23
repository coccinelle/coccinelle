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
open Syntax

let read_whole_file filename =

  (* Open the file in text mode, so that (under Windows) CRLF is converted to LF.
     This guarantees that one byte is one character and seems to be required in
     order to report accurate positions. *)

  let channel = open_in filename in

  (* The standard library functions [pos_in] and [seek_in] do not work correctly
     when CRLF conversion is being performed, so we abandon their use. (They were
     used to go and extract the text of semantic actions.) Instead we load the
     entire file into memory up front, and work with a string. *)

  (* The standard library function [in_channel_length] does not work correctly
     when CRLF conversion is being performed, so we do not use it to read the
     whole file. And the standard library function [Buffer.add_channel] uses
     [really_input] internally, so we cannot use it either. Bummer. *)

  let block_size = 16384 in
  let b = Buffer.create block_size in
  let s = String.create block_size in

  let rec loop () =
    let read = input channel s 0 block_size in
    if read > 0 then begin
      Buffer.add_substring b s 0 read;
      loop()
    end
  in

  loop();
  close_in channel;
  Buffer.contents b

let load_partial_grammar filename = 
  if Filename.check_suffix filename (if Settings.coq then ".vy" else ".mly") then
    Error.set_filename filename
  else
    Error.error [] (sprintf "argument file names should end in .mly. \"%s\" is not accepted." filename);
  try

    let contents = read_whole_file filename in
    Error.file_contents := Some contents;
    let lexbuf = Lexing.from_string contents in
    lexbuf.Lexing.lex_curr_p <-
	{ 
	  Lexing.pos_fname = filename; 
	  Lexing.pos_lnum  = 1;
	  Lexing.pos_bol   = 0; 
	  Lexing.pos_cnum  = 0
	};
    let grammar =
      { (Parser.grammar Lexer.main lexbuf) with ConcreteSyntax.pg_filename = filename }
    in
    Error.file_contents := None;

    (* If there were errors during parsing, stop. This has to be done
       explicitly here because the parser performs error recovery and
       does not die at the first error. One could even go further and
       attempt to work with the grammar in spite of the parse errors,
       but we choose not to. *)

    if Error.errors () then
      exit 1
    else
      grammar

  with Sys_error msg ->
    Error.error [] msg

let partial_grammars = 
  List.map load_partial_grammar Settings.filenames

let () =
  Time.tick "Lexing and parsing"

let parameterized_grammar = 
  PartialGrammar.join_partial_grammars partial_grammars

let grammar = 
  ParameterizedGrammar.expand parameterized_grammar

let () =
  Time.tick "Joining and expanding"

