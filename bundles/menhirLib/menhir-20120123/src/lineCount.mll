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

(* This simple function counts the number of newline characters
   in a string. *)

let newline = ('\010' | '\013' | "\013\010")

let ordinary = [^ '\010' '\013']+

rule count n = parse
| eof
    { n }
| newline
    { count (n + 1) lexbuf }
| ordinary
    { count n lexbuf }

