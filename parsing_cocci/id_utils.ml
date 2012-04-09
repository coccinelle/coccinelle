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


# 0 "./id_utils.ml"
(* evaluation of nested and and or using id-utils *)

module GC = Get_constants2

let evaluated = Hashtbl.create(100)

exception Out

let memoize exp vl = Hashtbl.add evaluated exp vl; vl

let rec interpret dir exp =
  let res = try Some (Hashtbl.find evaluated exp) with Not_found -> None in
  match res with
    Some x -> x
  | None ->
      memoize exp
	(match exp with
	  GC.Elem oo ->
	    let cmd =
	      Printf.sprintf
		"lid -f %s/.id-utils.index -l %s -S newline" dir oo in
	    Common.cmd_to_list cmd
	| GC.And l ->
	    let rec loop = function
		[] -> failwith "bad and"
	      | [x] -> interpret dir x
	      | x :: xs ->
		  (match interpret dir x with
		    [] -> raise Out
		  | resx ->
		      let resxs = loop xs in
		      Common.inter_set resx resxs) in
	    (try loop l with Out -> [])
	| GC.Or l ->
	    List.fold_left
	      (function prev -> function cur ->
		Common.union_set (interpret dir cur) prev)
	      [] l
	| _ -> failwith "not possible")
