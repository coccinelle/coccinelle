(*
 * Copyright 2012-2015, Inria
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


# 0 "./isomorphisms_c_c.ml"
open Common

(* When in a semantic patch there is f(X) ... f(X) we want to force
 * the two X to be equal in the concrete code, but we would like that
 * there be equal modulo some isomorphisms, so that the following
 * concrete code also match: f(a && b) g(); f(b && a)

 * Maybe would be easier to transform ast_c in ast_cocci and call the
 * iso engine of julia. *)

open Ast_c

let rec (iso_e_e: expression -> expression -> bool) = fun a b ->
  raise Todo
    (*
      let rec (=~=) a b =
      match (a, b) with
      | (Ident a, typa, iia), (Ident b, typb, iib) -> a = b
      | (Constant a, typa, iia), (Constant b, typb, iib) -> a = b
      | (FunCall  (ea, eas), typa, iia), (FunCall  (eb, ebs), typb, iib)        ->
      ea =~= eb &&
      List.length eas = List.length ebs &&
      List.for_all (fun (a, b) ->
      match (a, b) with
      | (Left ea, iia), (Left eb, iib) -> ea =~= eb
      | _ -> raise Todo
      )
      (zip eas ebs)
      | (Binary (ea1,Logical AndLog,ea2),typa, iia), (Binary (eb1,Logical AndLog, eb2), typb, iib) ->
      (ea1 =~= eb1  && ea2 =~= eb2)
      ||
      (ea1 =~= eb2  && ea2 =~= eb1)

      | _ -> raise Todo
      in
      a =~= b
    *)

and (iso_st_st: statement -> statement -> bool) = fun a b ->
  raise Todo
and (iso_t_t: fullType -> fullType -> bool) = fun a b ->
  raise Todo


(*
let _ = assert (iso_e_e
  (cexpression_of_string "a&&b")
  (cexpression_of_string "b&&a")
*)

