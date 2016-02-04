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


# 0 "./regexp_pcre.ml"
type regexp =
    Pcre of int (* Pcre.regexp *)
  | Str of Str.regexp

(* A table is used because PCRE regular expressions are not comparable.
They sit in constraints of rule_elems that get bound to exists v variables
in the matching process.  It would be expensive to strip them at runtime,
and complex to strip them statically, because both stripped and unstripped
versions would be needed in cocci_vs_c.  So instead we just replace them by
integers, which are comparable. *)

let pcre_table = Hashtbl.create 101
let pcre_ctr = ref 0

let pcre_support = ref true

let regexp string =
  if !pcre_support
  then
    begin
      let c = !pcre_ctr in
      pcre_ctr := !pcre_ctr + 1;
      Hashtbl.add pcre_table c (Pcre.regexp string);
      Pcre c
    end
  else Str (Str.regexp string)

let string_match regexp string =
  match regexp with
      Pcre regexp ->
	let regexp = Hashtbl.find pcre_table regexp in
	Pcre.pmatch ~rex:regexp string
    | Str regexp ->
      try
	ignore(Str.search_forward regexp string 0);
	true
      with _ -> false
