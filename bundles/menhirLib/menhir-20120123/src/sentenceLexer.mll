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

(* This lexer is used to read the sentences provided on the standard input
   channel when [--interpret] is enabled. *)

{

  open Lexing
  open SentenceParser
  open Grammar

  (* Updates the line counter, which is used in some error messages. *)

  let update_loc lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  (* A short-hand. *)

  let error1 lexbuf msg =
    Error.error (Positions.one (lexeme_start_p lexbuf)) msg

}

let newline   = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ';' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

rule lex = parse
  | (lowercase identchar *) as lid
      { try
	  let nt = Nonterminal.lookup lid in
	  if StringSet.mem lid Front.grammar.UnparameterizedSyntax.start_symbols then
	    NONTERMINAL nt
	  else
	    error1 lexbuf (Printf.sprintf "\"%s\" is not a start symbol." lid)
	with Not_found ->
	  error1 lexbuf (Printf.sprintf "\"%s\" is not a known non-terminal symbol." lid)
      }
  | (uppercase identchar *) as uid
      { try
	  TERMINAL (Terminal.lookup uid)
	with Not_found ->
	  error1 lexbuf (Printf.sprintf "\"%s\" is not a known terminal symbol." uid)
      }
  | whitespace
      { lex lexbuf }
  | newline
      { update_loc lexbuf; EOL }
  | eof
      { EOF }
  | ':'
      { COLON }
  | _
      { error1 lexbuf "unexpected character(s)." }

