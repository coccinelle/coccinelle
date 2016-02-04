(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

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
      "Usage: " ^ Filename.basename Sys.argv.(0) ^
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
