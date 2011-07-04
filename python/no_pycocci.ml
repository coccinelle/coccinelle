open Ast_c
open Common
open Pycocci_aux
module StringMap = Map.Make (String)

exception Pycocciexception

let python_support = false

let check_return_value v = failwith "no python"
let check_int_return_value v = failwith "no python"

let initialised = ref false

let cocci_file_name = ref ""

(* dealing with python modules loaded *)
let get_module module_name = failwith "no python"

let is_module_loaded module_name = failwith "no python"

let load_module module_name = failwith "no python"
(* end python module handling part *)

(* initialisation routines *)
let pycocci_init () = initialised := true

(*let _ = pycocci_init ()*)
(* end initialisation routines *)

(* python interaction *)
let split_fqn fqn = failwith "no python"

let pycocci_get_class_type fqn = failwith "no python"

let pycocci_instantiate_class fqn args = failwith "no python"

(* end python interaction *)

let inc_match = ref false
let exited = ref false

let include_match v = failwith "no python"

let sp_exit _ = failwith "no python"

let build_method (mname, camlfunc, args) pymodule classx classdict =
  failwith "no python"

let build_class cname parent methods pymodule = failwith "no python"

let has_environment_binding env name = failwith "no python"

let get_cocci_file args = failwith "no python"

let build_classes env = failwith "no python"

let build_variable name value = failwith "no python"

let contains_binding e (_,(r,m),_) = failwith "no python"

let construct_variables mv e = failwith "no python"

let construct_script_variables mv = failwith "no python"
let retrieve_script_variables mv = failwith "no python"

let set_coccifile cocci_file =
	cocci_file_name := cocci_file;
	()


let pyrun_simplestring s =
  failwith "no python"

let py_isinitialized () =
  failwith "no python"

let py_finalize () =
  failwith "no python"
