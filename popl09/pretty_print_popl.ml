(*
 * Copyright 2012-2014, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./pretty_print_popl.ml"
open Format
module Past = Ast_popl
module Ast = Ast_cocci

let start_block str =
  force_newline(); print_string "  "; open_box 0

let end_block str =
  close_box(); force_newline ()

(* --------------------------------------------------------------------- *)

let print_around printer term = function
    Ast.NOTHING -> printer term
  | Ast.BEFORE(bef,_) ->
      Pretty_print_cocci.print_anything "<<< " bef; printer term
  | Ast.AFTER(aft,_) ->
      printer term; Pretty_print_cocci.print_anything ">>> " aft
  | Ast.BEFOREAFTER(bef,aft,_) ->
      Pretty_print_cocci.print_anything "<<< " bef; printer term;
      Pretty_print_cocci.print_anything ">>> " aft

let mcode fn = function
    (x, _, Ast.MINUS(_,_,_,plus_stream), pos) ->
      print_string "-"; fn x;
      (match plus_stream with
	Ast.NOREPLACEMENT -> ()
      | Ast.REPLACEMENT(plus_stream,_) ->
	  Pretty_print_cocci.print_anything ">>> " plus_stream)
  | (x, _, Ast.CONTEXT(_,plus_streams), pos) ->
	print_around fn x plus_streams
  | (x, info, Ast.PLUS _, pos) -> fn x

(* --------------------------------------------------------------------- *)

let rec print_sequence = function
    Past.Seq(e,seq) -> print_element e; force_newline(); print_sequence seq
  | Past.Empty -> ()
  | Past.SExists((_,v),seq) -> print_string "exists "; print_string v;
      print_string " ."; force_newline(); print_sequence seq

and print_term = function
    Past.Atomic(term) -> Pretty_print_cocci.rule_elem "" term
  | Past.IfThen(test,thn,(_,_,_,aft)) ->
      print_term test; print_term thn;
      mcode (function _ -> ()) ((),Ast.no_info,aft,[])
  | Past.TExists((_,v),term) -> print_string "exists "; print_string v;
      print_string " ."; force_newline(); print_term term

and print_element = function
    Past.Term(term,_) -> print_term term
  | Past.Or(seq1,seq2) ->
      force_newline(); print_string "("; force_newline(); print_sequence seq1;
      print_string "|"; force_newline(); print_sequence seq2; print_string ")"
  | Past.DInfo(dots) ->
      start_block();
      print_dots dots;
      end_block()
  | Past.EExists((_,v),elem) -> print_string "exists "; print_string v;
      print_string " ."; force_newline(); print_element elem

and print_dots = function
    Past.Dots -> print_string "..."
  | Past.Nest(seq)-> print_string "<..."; start_block(); print_sequence seq;
      end_block(); print_string "...>"
  | Past.When(dots,seq) -> print_dots dots; print_string " when != ";
      open_box 0; print_sequence seq; close_box()

(* --------------------------------------------------------------------- *)

let pretty_print_e e =
  print_element e;
  print_newline()

let pretty_print sl =
  print_sequence sl;
  print_newline()

