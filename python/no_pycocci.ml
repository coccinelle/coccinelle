(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

open Ast_c
open Common
open Pycocci_aux
module StringMap = Map.Make (String)

exception Pycocciexception

let errmsg = "Semantic patch uses python, but Coccinelle has been compiled without Python support"

let python_support = false

let initialised = ref false

let cocci_file_name = ref ""

(* dealing with python modules loaded *)
let get_module module_name = failwith errmsg
(* end python module handling part *)

(* initialisation routines *)
let pycocci_init () = initialised := true

(*let _ = pycocci_init ()*)
(* end initialisation routines *)

let inc_match = ref false
let exited = ref false

let include_match v = failwith errmsg

let build_class cname parent methods pymodule = failwith errmsg

let build_classes env = failwith errmsg

let construct_variables mv e = failwith errmsg

let construct_script_variables mv = failwith errmsg
let retrieve_script_variables mv = failwith errmsg

let set_coccifile cocci_file =
	cocci_file_name := cocci_file;
	()

let pickle_variable v = failwith errmsg

let unpickle_variable v v' = failwith errmsg

let pyrun_simplestring s =
  failwith errmsg

let run s =
  failwith errmsg

let py_isinitialized () =
  failwith errmsg

let py_finalize () =
  failwith errmsg

let run_constraint args pos body =
  failwith errmsg

let run_fresh_id _ _ _ =
  failwith errmsg

let flush_stdout_and_stderr () = ()
