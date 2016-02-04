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


# 0 "./iso_compile.ml"
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types
module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* Detects where position variables can be present in the match of an
isomorphism.  This is allowed if all elements of an isomorphism have only
one token or if we can somehow match up equal tokens of all of the
isomorphic variants. *)

let sequence_tokens =
  let mcode x =
    (* sort of unpleasant to convert the token representation to a string
       but we can't make a list of mcodes otherwise because the types are all
       different *)
    [(Dumper.dump (Ast0.unwrap_mcode x),Ast0.get_pos_ref x)] in
  let donothing r k e = k e in
  let bind x y = x @ y in
  let option_default = [] in
  V0.flat_combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing

(* In general, we will get a list of lists:

[[tokens1;tokens2;tokens3];[tokens4;tokens5;tokens6];[tokens7;tokens8]]

If all of the lists of tokens contain only one element, we are done.

Otherwise, we focus on tokens1.  For each of its elements, if they are
present in all of the others, then a position is assigned, and if not then
a position is not.  The order of the elements in the other lists is
irrelevant; we just take the first unannotated element that matches.  Once
we are done with the elements of tokens1, we skip to tokens 4 and repeat,
including considering the one-element special case. *)

let pctr = ref 0
let get_p _ =
  let c = !pctr in
  pctr := c + 1;
  let name = ("",Printf.sprintf "p%d" c) in
  (* pos var just gives a name we can look up, used for historical reasons *)
  Ast0.HiddenVarTag
    ([Ast0.MetaPosTag(Ast0.MetaPos(Ast0.make_mcode name,[],Ast.PER))])

let process_info l =
   let rec loop previously_used = function
       [] -> ()
     | ((f::r)::xs) as a ->
	 let safe_add p pos =
	   (* don't add pos var where a pos var is already present *)
	   if Common.inter_set previously_used pos = [] then p::pos else pos in
	 let p =
	   if List.for_all (List.for_all (function e -> List.length e = 1)) a
	   then
	     let p = get_p() in
             List.iter
	       (List.iter
		  (List.iter (function (_,pos) -> pos := safe_add p !pos)))
	       a;
	     [p]
	   else
	     let all = r @ List.concat xs in
	     let rec find_first_available a = function
		 [] -> raise Not_found
	       | (str,pos)::xs ->
		   if str = a && Common.inter_set previously_used !pos = []
		   then pos
		   else find_first_available a xs in
	     List.fold_left
	       (function prev ->
		 function (str,pos) ->
		   if Common.inter_set previously_used !pos = []
		   then
		     try
		       let entries = List.map (find_first_available str) all in
		       let p = get_p() in
		       pos := p::!pos;
		       List.iter (function pos -> pos := p :: !pos) entries;
		       p::prev
		     with Not_found -> prev
		   (* otherwise already annotated *)
		   else prev)
	       [] f in
	 loop (p@previously_used) xs
     | _ -> failwith "bad iso" in
   loop l

(* Entry point *)

let process (metavars,alts,name) =
  let toks =
    List.map (List.map sequence_tokens.VT0.combiner_rec_anything) alts in
  process_info [] toks
