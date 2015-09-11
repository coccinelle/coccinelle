(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Format
module Past = Ast_popl

let start_block str =
  force_newline(); print_string "  "; open_box 0

let end_block str =
  close_box(); force_newline ()

(* --------------------------------------------------------------------- *)

let rec print_sequence = function
    Past.Seq(e,seq) -> print_element e; force_newline(); print_sequence seq
  | Past.Empty -> ()
  | Past.SExists((_,v),seq) -> print_string "exists "; print_string v;
      print_string " ."; force_newline(); print_sequence seq

and print_element = function
    Past.Term(term) -> Pretty_print_cocci.rule_elem "" term
  | Past.Or(seq1,seq2) ->
      force_newline(); print_string "("; force_newline(); print_sequence seq1;
      print_string "|"; force_newline(); print_sequence seq2; print_string ")"
  | Past.DInfo(dots,bef,aft) ->
      start_block();
      List.iter
	(function b -> print_string ">>>"; print_element b; force_newline())
	bef;
      print_dots dots;
      List.iter
	(function b -> force_newline(); print_string "<<<"; print_element b)
	aft;
      end_block()
  | Past.EExists((_,v),elem) -> print_string "exists "; print_string v;
      print_string " ."; force_newline(); print_element elem

and print_dots = function
    Past.Dots -> print_string "..."
  | Past.Nest(seq)-> print_string "<..."; start_block(); print_sequence seq;
      end_block(); print_string "...>"
  | Past.When(dots,seq) -> print_dots dots; print_string " when != ";
      open_box 0; print_sequence seq; close_box()
  | Past.DExists((_,v),dots) -> print_string "exists "; print_string v;
      print_string " ."; force_newline(); print_dots dots

(* --------------------------------------------------------------------- *)

let pretty_print_e e =
  print_element e;
  print_newline()

let pretty_print sl =
  print_sequence sl;
  print_newline()
