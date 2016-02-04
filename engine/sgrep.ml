(*
 * Copyright 2012, INRIA
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


# 0 "./sgrep.ml"
(* no longer used *)

type marker =
    NoMark | BefMark of string | AftMark of string
  | BefAftMark of string * string

let extract_sgrep_marker l =
  let rec inner_loop acc = function
      [] -> (acc,[])
    | Ast_cocci.SgrepStartTag(s)::rest ->
	(match acc with
	  NoMark -> inner_loop (BefMark(s)) rest
	| _ -> failwith "unexpected mark")
    | Ast_cocci.SgrepEndTag(s)::rest ->
	(match acc with
	  NoMark -> inner_loop (AftMark(s)) rest
	| BefMark(m) -> inner_loop (BefAftMark(m,s)) rest
	| _ -> failwith "unexpected mark")
    | x::rest ->
	let (acc,rest) = inner_loop acc rest in
	(acc,x::rest) in
  let (acc,l) =
    List.fold_left
      (function (acc,prev) ->
	function cur ->
	  let (acc,cur) = inner_loop acc cur in
	  (acc,cur::prev))
      (NoMark,[]) l in
  (acc,List.rev l)

let process_sgrep ii mck =
  let file = Ast_c.file_of_info ii in
  let line = Ast_c.line_of_info ii in
  let col = Ast_c.col_of_info ii in
  let str = Ast_c.str_of_info ii in
  match mck with
    Ast_cocci.MINUS(pos,inst,adj,repl) ->
      (match extract_sgrep_marker repl with
	(NoMark,_) -> mck
      |	(BefMark(marker),repl) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker file line col;
	  Ast_cocci.MINUS(pos,inst,adj,repl)
      |	(AftMark(marker),repl) ->
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    marker file line (col + String.length str);
	  Ast_cocci.MINUS(pos,inst,adj,repl)
      |	(BefAftMark(bmarker,amarker),repl) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    bmarker file line col;
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    amarker file line (col + String.length str);
	  Ast_cocci.MINUS(pos,inst,adj,repl))
  | Ast_cocci.CONTEXT(pos,Ast_cocci.NOTHING) -> mck
  | Ast_cocci.CONTEXT(pos,Ast_cocci.BEFORE(bef,c)) ->
      (match extract_sgrep_marker bef with
	(NoMark,_) -> mck
      |	(BefMark(marker),[]) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker file line col;
	  Ast_cocci.CONTEXT(pos,Ast_cocci.NOTHING)
      |	(BefMark(marker),bef) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker file line col;
	  Ast_cocci.CONTEXT(pos,Ast_cocci.BEFORE(bef,c))
      |	_ -> failwith "after not possible")
  | Ast_cocci.CONTEXT(pos,Ast_cocci.AFTER(aft,c)) ->
      (match extract_sgrep_marker aft with
	(NoMark,_) -> mck
      |	(AftMark(marker),[]) ->
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    marker file line (col + String.length str);
	  Ast_cocci.CONTEXT(pos,Ast_cocci.NOTHING)
      |	(AftMark(marker),aft) ->
	  Printf.printf "Match on line %s ending at %s: line %d offset %d\n"
	    marker file line (col + String.length str);
	  Ast_cocci.CONTEXT(pos,Ast_cocci.AFTER(aft,c))
      |	_ -> failwith "before not possible")
  | Ast_cocci.CONTEXT(pos,Ast_cocci.BEFOREAFTER(bef,aft,c)) ->
      (match extract_sgrep_marker bef with
	(NoMark,_) ->
	  (match extract_sgrep_marker aft with
	    (NoMark,_) -> mck
	  | (AftMark(marker),[]) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker file line (col + String.length str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.BEFORE(bef,c))
	  | (AftMark(marker),aft) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker file line (col + String.length str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.BEFOREAFTER(bef,aft,c))
	  | _ -> failwith "before not possible")
      | (BefMark(marker),[]) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker file line col;
	  (match extract_sgrep_marker aft with
	    (NoMark,_) -> Ast_cocci.CONTEXT(pos,Ast_cocci.AFTER(aft,c))
	  | (AftMark(marker),[]) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker file line (col + String.length str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.NOTHING)
	  | (AftMark(marker),aft) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker file line (col + String.length str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.AFTER(aft,c))
	  | _ -> failwith "before not possible")
      | (BefMark(marker),bef) ->
	  Printf.printf "Match on line %s starting at %s: line %d offset %d\n"
	    marker file line col;
	  (match extract_sgrep_marker aft with
	    (NoMark,_) ->
	      Ast_cocci.CONTEXT(pos,Ast_cocci.BEFOREAFTER(bef,aft,c))
	  | (AftMark(marker),[]) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker file line (col + String.length str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.BEFORE(bef,c))
	  | (AftMark(marker),aft) ->
	      Printf.printf
		"Match on line %s ending at %s: line %d offset %d\n"
		marker file line (col + String.length str);
	      Ast_cocci.CONTEXT(pos,Ast_cocci.BEFOREAFTER(bef,aft,c))
	  | _ -> failwith "before not possible")
      |	_ -> failwith "after not possible")
  | _ -> failwith "unexpected plus code"
