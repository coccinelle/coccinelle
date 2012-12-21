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


# 0 "./commas_on_lists.ml"
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

(* Add commas in init lists or exp lists, if needed.  This must be done
before the adjacency calculation so that the commas get the right
adjacency numbers.  This is needed for correct formatting in unparse_c.ml *)

(* commas in dotted lists, here due to polymorphism restrictions *)

let add_comma is_comma is_dots make_comma itemlist =
  match Ast0.unwrap itemlist with
    Ast0.DOTS(x) ->
      (match List.rev x with
        [] -> itemlist
(* Not sure if comma is needed if the list is just ...; leave it there for      
now. See list_matcher in cocci_vs_c.ml in first try_matches case. *)
(*      |       [e] when is_dots e -> itemlist*)
      |	 e::es ->
          if is_comma e
          then itemlist
          else
            let comma =
              match Ast0.get_mcodekind e with
                Ast0.MINUS(_) -> (Ast0.make_minus_mcode ",")
              |	 _ -> (Ast0.make_mcode ",") in
	        Ast0.rewrap itemlist
              (Ast0.DOTS
                 (List.rev (Ast0.rewrap e (make_comma comma) :: (e::es)))))
  |  _ -> failwith "not possible"

let add_exp_comma =
  add_comma
    (function x -> match Ast0.unwrap x with Ast0.EComma _ -> true | _ -> false)
    (function x -> match Ast0.unwrap x with Ast0.Edots _  -> true | _ -> false)
    (function x -> Ast0.EComma x)

and add_init_comma =
  add_comma
    (function x -> match Ast0.unwrap x with Ast0.IComma _ -> true | _ -> false)
    (function x -> match Ast0.unwrap x with Ast0.Idots _  -> true | _ -> false)
    (function x -> Ast0.IComma x)

(* --------------------------------------------------------------------- *)
(* special cases for terms that contain comma-separated lists where the
trailing comma is allowed but not required *)

let base_typeC r k t =
  let t = k t in
  match Ast0.unwrap t with
    Ast0.EnumDef(ty,lb,ids,rb) ->
      let ids = add_exp_comma ids in
      Ast0.rewrap t (Ast0.EnumDef(ty,lb,ids,rb))
  | _ -> t

let initialiser r k i =
  let i = k i in
  match Ast0.unwrap i with
    Ast0.InitList(lb,initlist,rb,ordered) ->
      let initlist = add_init_comma initlist in
      Ast0.rewrap i (Ast0.InitList(lb,initlist,rb,ordered))
  | _ -> i

let process p =
  let fn =
    V0.rebuilder
      {V0.rebuilder_functions with
	VT0.rebuilder_tyfn = base_typeC;
	VT0.rebuilder_initfn = initialiser} in
  List.map fn.VT0.rebuilder_rec_top_level p
