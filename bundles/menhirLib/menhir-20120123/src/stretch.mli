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

(* $Id: stretch.mli,v 1.4 2005/12/01 16:20:07 regisgia Exp $ *)

(* A stretch is a fragment of a source file. It holds the file name,
   the line number, and the line count (that is, the length) of the
   fragment. These are used for generating #line directives when the
   fragment is copied to an output file. It also holds the textual
   content of the fragment, as a string. The [raw_content] field holds
   the text that was found in the source file, while the [content]
   field holds the same text after transformation by the lexer (which
   substitutes keywords, inserts padding, etc.). *)

type t = {
    stretch_filename	: string;
    stretch_linenum	: int;
    stretch_linecount	: int;
    stretch_raw_content	: string;
    stretch_content	: string;
    stretch_keywords	: Keyword.keyword Positions.located list
  } 

(* An Objective Caml type is either a stretch (if it was found in some
   source file) or a string (if it was inferred via [Infer]). *)

type ocamltype =
  | Declared of t
  | Inferred of string

