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


# 0 "./git_grep.ml"
exception Failed

let interpret dir query suffixes =
  let collect query =
    let cmd =
      Printf.sprintf "cd %s; git grep -l -w %s -- %s" dir query suffixes in
    let (res,code) = Common.cmd_to_list_and_status cmd in
    if code = Unix.WEXITED 0 || code = Unix.WEXITED 1
    then res
    else raise Failed in
  try
    match List.map collect query with
      [] -> failwith "not possible"
    | x::xs ->
	let res = List.fold_left Common.inter_set x xs in
	Some(List.map (function x -> dir^"/"^x) res)
  with Failed -> None
