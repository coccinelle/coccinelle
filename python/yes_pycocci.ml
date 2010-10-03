(*
* Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
* Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
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


open Ast_c
open Common
open Pycaml
open Pycocci_aux
module StringMap = Map.Make (String)

exception Pycocciexception

let check_return_value v =
  if v = (pynull ()) then 
	  (pyerr_print ();
	  raise Pycocciexception)
  else ()
let check_int_return_value v =
  if v = -1 then
	  (pyerr_print ();
	  raise Pycocciexception)
  else ()

let initialised = ref false

let coccinelle_module = ref (pynone ())
let cocci_file_name = ref ""

(* dealing with python modules loaded *)
let module_map = ref (StringMap.add "__main__" (pynone ()) StringMap.empty)

let get_module module_name =
  StringMap.find module_name (!module_map)

let is_module_loaded module_name =
  try
    let _ = get_module module_name in
    true
  with Not_found -> false

let load_module module_name =
  if not (is_module_loaded module_name) then
    let m = pyimport_importmodule module_name in
    check_return_value m;
    (module_map := (StringMap.add module_name m (!module_map));
    m)
  else get_module module_name
(* end python module handling part *)

(* initialisation routines *)
let pycocci_init () =
  (* initialize *)
  if not !initialised then (
  initialised := true;
  Unix.putenv "PYTHONPATH"
      (Printf.sprintf "%s/coccinelle" (Unix.getenv "HOME"));
  let _ = if not (py_isinitialized () != 0) then 
  	(if !Flag.show_misc then Common.pr2 "Initializing python\n%!"; 
	py_initialize()) in

  (* set argv *)
  let argv0 = Printf.sprintf "%s%sspatch" (Sys.getcwd ()) (match Sys.os_type with "Win32" -> "\\" | _ -> "/") in
  let _ = pycaml_setargs argv0 in

  coccinelle_module := (pymodule_new "coccinelle");
  module_map := StringMap.add "coccinelle" !coccinelle_module !module_map;
  let _ = load_module "coccilib.elems" in
  let _ = load_module "coccilib.output" in
  ()) else

  ()

(*let _ = pycocci_init ()*)
(* end initialisation routines *)

(* python interaction *)
let split_fqn fqn =
  let last_period = String.rindex fqn '.' in
  let module_name = String.sub fqn 0 last_period in
  let class_name = String.sub fqn (last_period + 1) (String.length fqn - last_period - 1) in
  (module_name, class_name)

let pycocci_get_class_type fqn =
  let (module_name, class_name) = split_fqn fqn in
  let m = get_module module_name in
  let attr = pyobject_getattrstring(m, class_name) in
  check_return_value attr;
  attr

let pycocci_instantiate_class fqn args =
  let class_type = pycocci_get_class_type fqn in
  let obj = pyobject_callobject(class_type, args) in
  check_return_value obj;
  obj

(* end python interaction *)

let inc_match = ref true

let include_match v =
  let truth = pyobject_istrue (pytuple_getitem (v, 1)) in
  check_int_return_value truth;
  inc_match := truth != 0;
  pynone ()

let build_method (mname, camlfunc, args) pymodule classx classdict =
  let cmx = pymethod_new(pywrap_closure camlfunc, args, classx) in
  let v = pydict_setitemstring(classdict, mname, cmx) in
  check_int_return_value v;
  ()

let build_class cname parent methods pymodule =
  let cd = pydict_new() in
  check_return_value cd;
  let cx = pyclass_new(pytuple_fromsingle (pycocci_get_class_type parent), cd, pystring_fromstring cname) in
  check_return_value cx;
  List.iter (function meth -> build_method meth pymodule cx cd) methods;
  let v = pydict_setitemstring(pymodule_getdict pymodule, cname, cx) in
  check_int_return_value v;
  (cd, cx)

let has_environment_binding env name =
  let a = pytuple_toarray name in
  let (rule, name) = (Array.get a 1, Array.get a 2) in
  let orule = pystring_asstring rule in
  let oname = pystring_asstring name in
  let e = List.exists (function (x,y) -> orule = x && oname = y) env in
  if e then pytrue () else pyfalse ()

let pyoutputinstance = ref (pynone ())
let pyoutputdict = ref (pynone ())

let get_cocci_file args =
	pystring_fromstring (!cocci_file_name)

let build_classes env =
	let _ = pycocci_init () in
	let module_dictionary = pyimport_getmoduledict() in
        coccinelle_module := pymodule_new "coccinelle";
	let mx = !coccinelle_module in
	inc_match := true;
        let (cd, cx) = build_class "Cocci" (!Flag.pyoutput) 
		[("include_match", include_match, (pynull()));
		 ("has_env_binding", has_environment_binding env, (pynull()))] mx in
	pyoutputinstance := cx;
	pyoutputdict := cd;
	let v1 = pydict_setitemstring(module_dictionary, "coccinelle", mx) in
	check_int_return_value v1;
        let mypystring = pystring_fromstring !cocci_file_name in
        let v2 = pydict_setitemstring(cd, "cocci_file", mypystring) in
	check_int_return_value v2;
        ()

let build_variable name value =
  let mx = !coccinelle_module in
  check_int_return_value (pydict_setitemstring(pymodule_getdict mx, name, value))

let contains_binding e (_,(r,m)) =
  try
    let _ = List.find (function ((re, rm), _) -> r = re && m = rm) e in true
  with Not_found -> false

let construct_variables mv e =
  let find_binding (r,m) =
    try
      let elem = List.find (function ((re,rm),_) -> r = re && m = rm) e in
      Some elem
    with Not_found -> None
  in

  let instantiate_Expression(x) =
    let str = pystring_fromstring (Pycocci_aux.exprrep x) in
    pycocci_instantiate_class "coccilib.elems.Expression" (pytuple_fromsingle (str))
  in

  let instantiate_Identifier(x) =
    let str = pystring_fromstring x in
    pycocci_instantiate_class "coccilib.elems.Identifier" (pytuple_fromsingle (str))
  in

  List.iter (function (py,(r,m)) ->
    match find_binding (r,m) with
      None -> ()
    | Some (_, Ast_c.MetaExprVal ((expr, _), info_list)) ->
       let expr_repr = instantiate_Expression(expr) in
       let _ = build_variable py expr_repr in
       ()
    | Some (_, Ast_c.MetaIdVal id) ->
       let id_repr = instantiate_Identifier(id) in
       let _ = build_variable py id_repr in
       ()
    | Some (_, Ast_c.MetaPosValList l) ->
       let locs =
	 List.map
	   (function (fname,current_element,(line,col),(line_end,col_end)) ->
		pycocci_instantiate_class "coccilib.elems.Location" (pytuple6
		(pystring_fromstring fname,pystring_fromstring current_element,
		pystring_fromstring (Printf.sprintf "%d" line),
		pystring_fromstring (Printf.sprintf "%d" col),
		pystring_fromstring (Printf.sprintf "%d" line_end),
		pystring_fromstring (Printf.sprintf "%d" col_end)))) l in
       let pylocs = pytuple_fromarray (Array.of_list locs) in
       let _ = build_variable py pylocs in
       ()
    | Some (_,binding) ->
       let _ = build_variable py (pystring_fromstring (Pycocci_aux.stringrep binding))
       in ()
    ) mv;

  ()

let set_coccifile cocci_file =
	cocci_file_name := cocci_file;
	()


let pyrun_simplestring s = 
  Pycaml.pyrun_simplestring s

let py_isinitialized () = 
  Pycaml.py_isinitialized ()


let py_finalize () =
  Pycaml.py_finalize ()
