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


# 0 "./main.ml"
(* ----------------------------------------------------------------------- *)
(* Entry point *)

let file = ref ""
let isofile = ref None
let verbose = ref true

let anonymous s = if !file = "" then file := s else isofile := Some s

let speclist = [("-v", Arg.Set verbose, "print parse result")]

let usage =
  Printf.sprintf "Usage: %s [options] <filename> \nOptions are:"
    (Filename.basename Sys.argv.(0))

let main _ =
  begin
  Arg.parse speclist anonymous usage;
  (* Parse_cocci.parse_and_merge !file; *)
  if !file = "" then failwith "filename required";
  Parse_cocci.process !file !isofile !verbose
  end

let _ = main ()
