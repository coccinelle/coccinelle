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


# 0 "./adjacency.ml"
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

let compute_adjacency p =
  let counter = ref 0 in
  let mcode (a,b,c,d,e,_) = (a,b,c,d,e,!counter) in
  let string_mcode ((str,_,info,mc,_,_) as x) =
    match str with
      "..." | "<..." | "...>" | "<+..." | "...+>" ->
	(match mc with
	  Ast0.MINUS _ -> mcode x
	| Ast0.CONTEXT _ -> counter := !counter + 1; x
	| _ -> failwith "unexpected mcode for ...")
    | _ -> mcode x in
  let fn =
    V0.rebuilder
      {V0.rebuilder_functions with
	VT0.rebuilder_meta_mcode = mcode;
	VT0.rebuilder_string_mcode = string_mcode;
	VT0.rebuilder_const_mcode = mcode;
	VT0.rebuilder_assign_mcode = mcode;
	VT0.rebuilder_fix_mcode = mcode;
	VT0.rebuilder_unary_mcode = mcode;
	VT0.rebuilder_binary_mcode = mcode;
	VT0.rebuilder_cv_mcode = mcode;
	VT0.rebuilder_sign_mcode = mcode;
	VT0.rebuilder_struct_mcode = mcode;
	VT0.rebuilder_storage_mcode = mcode;
	VT0.rebuilder_inc_mcode = mcode;} in
  List.map fn.VT0.rebuilder_rec_top_level p

