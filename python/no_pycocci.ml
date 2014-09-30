(*
 * Copyright 2012-2014, INRIA
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


# 0 "./no_pycocci.ml"
open Ast_c
open Common
open Pycocci_aux
module StringMap = Map.Make (String)

exception Pycocciexception

let errmsg = "Semantic patch uses python, but Coccinelle has been compiled without Python support"

let python_support = false

let check_return_value v = failwith errmsg
let check_int_return_value v = failwith errmsg

let initialised = ref false

let cocci_file_name = ref ""

(* dealing with python modules loaded *)
let get_module module_name = failwith errmsg

let is_module_loaded module_name = failwith errmsg

let load_module module_name = failwith errmsg
(* end python module handling part *)

(* initialisation routines *)
let pycocci_init () = initialised := true

(*let _ = pycocci_init ()*)
(* end initialisation routines *)

(* python interaction *)
let split_fqn fqn = failwith errmsg

let pycocci_get_class_type fqn = failwith errmsg

let pycocci_instantiate_class fqn args = failwith errmsg

(* end python interaction *)

let inc_match = ref false
let exited = ref false

let include_match v = failwith errmsg

let sp_exit _ = failwith errmsg

let build_method (mname, camlfunc, args) pymodule classx classdict =
  failwith errmsg

let build_class cname parent methods pymodule = failwith errmsg

let has_environment_binding env name = failwith errmsg

let get_cocci_file args = failwith errmsg

let build_classes env = failwith errmsg

let build_variable name value = failwith errmsg

let contains_binding e (_,(r,m),_) = failwith errmsg

let construct_variables mv e = failwith errmsg

let construct_script_variables mv = failwith errmsg
let retrieve_script_variables mv = failwith errmsg

let set_coccifile cocci_file =
	cocci_file_name := cocci_file;
	()


let pyrun_simplestring s =
  failwith errmsg

let py_isinitialized () =
  failwith errmsg

let py_finalize () =
  failwith errmsg
