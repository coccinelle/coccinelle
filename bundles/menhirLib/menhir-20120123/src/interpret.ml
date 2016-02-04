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

(* This module is in charge of handling the [--interpret] option,
   if it is present. *)

open Grammar

(* --------------------------------------------------------------------------- *)

(* A sentence is a pair of an optional non-terminal start symbol and a
   list of terminal symbols. *)

type sentence =
    Nonterminal.t option * Terminal.t list

(* --------------------------------------------------------------------------- *)

(* [stream] turns a finite list of terminals into a stream of terminals. *)

exception EndOfStream

let stream (toks : Terminal.t list) : unit -> Terminal.t * Lexing.position * Lexing.position =
  let toks = ref toks in
  fun () ->

    let tok =
      match !toks with
      | tok :: more ->

	  (* Take a token off the list, and return it. *)

	  toks := more;
	  tok

      | [] ->

	  (* The finite list has been exhausted. Here, two plausible behaviors
	     come to mind.

	     The first behavior consists in raising an exception. In that case,
	     we are creating a finite stream, and it is up to the parser to not
	     read past its end.

	     The second behavior consists in returning a designated token. In
	     that case, we are creating an infinite, eventually constant,
	     stream.

	     The choice between these two behaviors is somewhat arbitrary;
	     furthermore, in the second case, the choice of the designated
	     token is arbitrary as well. Here, we adopt the second behavior if
	     and only if the grammar has an EOF token, and we use EOF as the
	     designated token. Again, this is arbitrary, and could be changed
	     in the future. *)

	  match Terminal.eof with
	  | Some eof ->
	      eof
	  | None ->
	      raise EndOfStream

    in

    (* For now, return dummy positions. *)

    tok, Lexing.dummy_pos, Lexing.dummy_pos

(* --------------------------------------------------------------------------- *)

(* [interpret] interprets a sentence. *)

let interpret ((nto, toks) : sentence) : unit =

  (* Check whether a start symbol was provided. If not, use the grammar's
     unique start symbol, if there is one. *)

  (* The code that finds the unique start symbol is not very pretty. *)

  let nt =
    match nto, ProductionMap.is_singleton Lr1.entry with
    | Some nt, _ ->
	nt
    | None, Some (prod, _) ->
	begin match Production.classify prod with
	| Some nt ->
	    nt
	| None ->
	    assert false
	end
    | None, None ->
	Error.error []
	  "Because the grammar has multiple start symbols, each of the\n\
           sentences provided on the standard input channel must be of the\n\
           form: <start symbol>: <token>*"
  in

  (* Run the reference interpreter. This can produce a concrete syntax tree
     ([Some cst]), fail with a parser error ([None]), or fail with a lexer error
     ([EndOfStream]). *)

  (* In either case, we produce just one line of output, so it should be clear
     to the user which outcomes correspond to which sentences (should multiple
     sentences be supplied). *)

  begin try
    match
      MenhirLib.Convert.Simplified.traditional2revised
	(ReferenceInterpreter.interpret Settings.trace nt)
	(stream toks)
    with

    | Some cst ->

	(* Success. *)

	Printf.printf "ACCEPT";
	if Settings.interpret_show_cst then begin
	  print_newline();
	  Cst.show stdout cst
	end

    | None ->

	(* Parser failure. *)

	Printf.printf "REJECT"

  with EndOfStream ->

    (* Lexer failure. *)
    
    Printf.printf "OVERSHOOT"

  end;
  print_newline()

(* --------------------------------------------------------------------------- *)

(* If [--interpret] is set, interpret the sentences found on the standard
   input channel, then stop, without generating a parser. *)

open Lexing

let () =
  if Settings.interpret then begin

    (* Read a series of sentences from the standard input channel. *)

    (* For more comfortable interaction, we interpret each sentence
       as soon as it is read. *)

    let lexbuf =
      from_channel stdin
    in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(stdin)" };

    let read () =
      try
	SentenceParser.sentence SentenceLexer.lex lexbuf
      with Parsing.Parse_error ->
	Error.error (Positions.lexbuf lexbuf) "Ill-formed input sentence."
    in

    let rec loop () =
      match read() with
      | None ->
	  exit 0
      | Some sentence ->
	  interpret sentence;
	  loop()
    in
    loop()

  end
