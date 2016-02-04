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


# 0 "./generate_dependencies.ml"
open Common

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let generate_dependencies dir =
  let c_info =
    Common.glob (Filename.concat dir "*.[c]")
    +> List.map (fun file ->
      let (x,_) = Parse_c.parse_cache file in
      let defined = C_info.defined_stuff x in
      let used = C_info.used_stuff x in
      let extra = C_info.extra_stuff x in
      C_info.adjust_used_only_external used defined;
      file, { C_info.used = used; defined = defined; is_module = extra}
    ) in
  let global = C_info.mk_global_definitions_index c_info in
  c_info +> List.iter (fun (file, used_defined) ->
    pr2 ("HANDLING : " ^ file);
    C_info.print_entities used_defined.C_info.used;
  );
  C_info.check_no_duplicate_global_definitions global;
  let g = C_info.build_graph c_info global
    (Filename.concat dir "depgraph.dot") in
  C_info.generate_makefile g (Filename.concat dir "depcocci.dep")




(*
  let path =
    match xs with
    | [] -> "/home/pad/kernels/git/linux-2.6/drivers/net"
    | [x] -> x
    | _ -> failwith "too much path"
  in

  let dirs =
    if dir
    then Common.cmd_to_list ("find " ^ path ^ " -type d") +> Kbuild.adjust_dirs
    else [path]
  in
  dirs +> List.iter (fun dir ->
*)


(*
let test_yyy () =
  Sys.chdir "/home/pad/kernels/git/linux-2.6";
  let path="drivers/net" in

  let c_info =
    Common.cmd_to_list ("find " ^ path ^ " -name \"*.c\" ")
    +> List.map (fun file ->
      let x = cprogram_of_file_cached file in
      let defined = defined_stuff x in
      let used = used_stuff x in
      let extra = extra_stuff x in
      adjust_used_only_external used defined;
      file, { used = used; defined = defined; is_module = extra}
    ) in
  let global = mk_global_definitions_index c_info in
  c_info +> List.iter (fun (file, used_defined) ->
    pr2 ("HANDLING : " ^ file);
    print_entities used_defined.used;
  );
  check_no_duplicate_global_definitions global
    (*build_graph c_info global (Filename.concat dir "depgraph.dot");*)
*)


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =
  begin
    let args = ref [] in
    let options = [
    ] in
    let usage_msg =
      "Usage: " ^ basename Sys.argv.(0) ^
        " <dir> [options]" ^ "\n" ^ "Options are:"
    in

    Arg.parse (Arg.align options) (fun x -> args := x::!args) usage_msg;
    args := List.rev !args;

    (match (!args) with
    | [x] ->
        generate_dependencies x
    | _ -> Arg.usage (Arg.align options) usage_msg;
    )
  end

(*****************************************************************************)
let _ =
  main ()

