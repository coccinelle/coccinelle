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

(* This code analyzes the output of [ocamldep] and returns the list
   of [.cmi] files that the [.cmo] file depends on. *)

{

  open Lexing

  let fail lexbuf =
    Error.error []
      (Printf.sprintf
	 "failed to make sense of ocamldep's output (character %d)."
	 lexbuf.lex_curr_p.pos_cnum)

}

let newline = ('\n' | '\r' | "\r\n")

let whitespace = ( ' ' | '\t' | ('\\' newline) )

let entrychar = [^ '\n' '\r' '\t' ' ' '\\' ':' ]

let entry = ((entrychar+ as basename) ".cm" ('i' | 'o' | 'x') as filename)

(* [main] recognizes a sequence of lines, where a line consists of an
   entry, followed by a colon, followed by a list of entries. *)

rule main = parse
| eof
    { [] }
| entry ":"
    { let bfs = collect [] lexbuf in
      ((basename, filename), bfs) :: main lexbuf }
| _
    { fail lexbuf }

(* [collect] recognizes a list of entries, separated with spaces and
   ending in a newline. *)

and collect bfs = parse
| whitespace+ entry
    { collect ((basename, filename) :: bfs) lexbuf }
| whitespace* newline
    { bfs }
| _
| eof
    { fail lexbuf }

