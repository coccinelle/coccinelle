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


# 0 "./extract_c_and_res.ml"
open Common

(*****************************************************************************)
(*  *)
(*****************************************************************************)
(* requirments:
 *  - extract from one git commit, or from a set of git commits
 *  - files from drivers/ and also from other directories now
 *  - .c and .h, local .h and also not local .h
 *  - files that were part of the patch, and so modified, but also
 *    other files (especially .h) to get more type information
 *
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let ex_kmallocmemset = "dd00cc486ab1c17049a535413d1751ef3482141c"
let ex_new_driver = "3faa1ffb4f4be7d10715f4b003ff7b27d14eae26"
let ex_delete_driver = "4d8506b806cc726c96db1c1a55edfb2da52217a9"

let main () =
  begin
    let args = ref [] in
    let options = [
    ] in
    let usage_msg =
      "Usage: " ^ basename Sys.argv.(0) ^
        " <file> [options]" ^ "\n" ^ "Options are:"
    in

    Arg.parse (Arg.align options) (fun x -> args := x::!args) usage_msg;
    args := List.rev !args;

    (match (!args) with
    | [x] -> pr x
    | _ -> Arg.usage (Arg.align options) usage_msg;
    )
  end

(*****************************************************************************)
let _ =
  main ()

