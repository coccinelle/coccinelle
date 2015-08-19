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


# 0 "./yes_pycocci.ml"
open Ast_c
open Common
open Pycaml
open Pycocci_aux
module StringMap = Map.Make (String)

exception Pycocciexception

let python_support = true

(* ------------------------------------------------------------------- *)
(* The following definitions are from
http://patches.ubuntu.com/by-release/extracted/debian/c/coccinelle/0.1.5dbs-2/01-system-pycaml
as well as _pycocci_setargs *)

let _pycocci_none () =
  let builtins = pyeval_getbuiltins () in
  pyobject_getitem (builtins, pystring_fromstring "None")

let _pycocci_true () =
  let builtins = pyeval_getbuiltins () in
  pyobject_getitem (builtins, pystring_fromstring "True")

let _pycocci_false () =
  let builtins = pyeval_getbuiltins () in
  pyobject_getitem (builtins, pystring_fromstring "False")

let _pycocci_tuple6 (a,b,c,d,e,f) =
  pytuple_fromarray ([|a; b; c; d; e; f|])

(* ------------------------------------------------------------------- *)

let check_return_value msg v =
  if v = (pynull ()) then
	  (pyerr_print ();
	  Common.pr2 ("while " ^ msg ^ ":");
	  raise Pycocciexception)
  else ()
let check_int_return_value msg v =
  if v = -1 then
	  (pyerr_print ();
          Common.pr2 ("while " ^ msg ^ ":");
	  raise Pycocciexception)
  else ()

let initialised = ref false

let coccinelle_module = ref (_pycocci_none ())
let cocci_file_name = ref ""

(* dealing with python modules loaded *)
let module_map = ref (StringMap.add "__main__" (_pycocci_none ()) StringMap.empty)

let get_module module_name =
  StringMap.find module_name (!module_map)

let is_module_loaded module_name =
  try
    let _ = get_module module_name in
    true
  with Not_found -> false

let load_module module_name =
  if not (is_module_loaded module_name) then
    (* let _ = Sys.command("python3 -c 'import " ^ module_name ^ "'") in *)
    let m = pyimport_importmodule module_name in
    check_return_value ("importing module " ^ module_name) m;
    (module_map := (StringMap.add module_name m (!module_map));
    m)
  else get_module module_name
(* end python module handling part *)

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
  check_return_value "obtaining a python class type" attr;
  attr

let pycocci_instantiate_class fqn args =
  let class_type = pycocci_get_class_type fqn in
  let obj =
    pyeval_callobjectwithkeywords(class_type, args, pynull()) in
  check_return_value "instantiating a python class" obj;
  obj

(* end python interaction *)

let inc_match = ref true
let exited = ref false

let include_match v =
  let truth = pyobject_istrue (pytuple_getitem (v, 1)) in
  check_int_return_value "testing include_match" truth;
  inc_match := truth != 0;
  _pycocci_none ()

let sp_exit _ =
  exited := true;
  _pycocci_none ()

let build_method (mname, camlfunc, args) pymodule classx classdict =
  let cmx = pymethod_new(pywrap_closure camlfunc, args, classx) in
  let v = pydict_setitemstring(classdict, mname, cmx) in
  check_int_return_value ("building python method " ^ mname) v;
  ()

let build_class cname parent methods pymodule =
  let cd = pydict_new() in
  check_return_value "creating a new python dictionary" cd;
  let cx = pyclass_new(pytuple_fromsingle (pycocci_get_class_type parent), cd,
		       pystring_fromstring cname) in
  check_return_value "creating a new python class" cx;
  List.iter (function meth -> build_method meth pymodule cx cd) methods;
  let v = pydict_setitemstring(pymodule_getdict pymodule, cname, cx) in
  check_int_return_value ("adding python class " ^ cname) v;
  (cd, cx)

let the_environment = ref []

let has_environment_binding name =
  let a = pytuple_toarray name in
  let (rule, name) = (Array.get a 1, Array.get a 2) in
  let orule = pystring_asstring rule in
  let oname = pystring_asstring name in
  let e = List.exists (function (x,y) -> orule = x && oname = y)
      !the_environment in
  if e then _pycocci_true () else _pycocci_false ()

let pyoutputinstance = ref (_pycocci_none ())
let pyoutputdict = ref (_pycocci_none ())

let get_cocci_file args =
  pystring_fromstring (!cocci_file_name)

(* initialisation routines *)
let _pycocci_setargs argv0 =
  let argv =
    pysequence_list (pytuple_fromsingle (pystring_fromstring argv0)) in
  let sys_mod = load_module "sys" in
  pyobject_setattrstring (sys_mod, "argv", argv)

let pycocci_init () =
  (* initialize *)
  if not !initialised then (
  initialised := true;
  (* use python_path_base as default (overridable) dir for coccilib *)
  let python_path_base = Printf.sprintf "%s/coccinelle" (Unix.getenv "HOME") in
  let python_path = try Unix.getenv "PYTHONPATH" ^ ":" ^ python_path_base
                    with Not_found -> python_path_base in
  Unix.putenv "PYTHONPATH" python_path;
  let _ = if not (py_isinitialized () != 0) then
  	(if !Flag.show_misc then Common.pr2 "Initializing python\n%!";
	py_initialize()) in

  (* set argv *)
  let argv0 = Sys.executable_name in
  let _ = _pycocci_setargs argv0 in

  coccinelle_module := (pymodule_new "coccinelle");
  module_map := StringMap.add "coccinelle" !coccinelle_module !module_map;
  let _ = load_module "coccilib.elems" in
  let _ = load_module "coccilib.output" in

  let module_dictionary = pyimport_getmoduledict() in
  coccinelle_module := pymodule_new "coccinelle";
  let mx = !coccinelle_module in
  let (cd, cx) = build_class "Cocci" (!Flag.pyoutput)
      [("exit", sp_exit, (pynull()));
	("include_match", include_match, (pynull()));
	("has_env_binding", has_environment_binding, (pynull()))] mx in
  pyoutputinstance := cx;
  pyoutputdict := cd;
  let v1 = pydict_setitemstring(module_dictionary, "coccinelle", mx) in
  check_int_return_value "adding coccinelle python module" v1;
  let mypystring = pystring_fromstring !cocci_file_name in
  let v2 = pydict_setitemstring(cd, "cocci_file", mypystring) in
  check_int_return_value "adding python field cocci_file" v2;
  ()) else
  ()

(*let _ = pycocci_init ()*)
(* end initialisation routines *)

let added_variables = ref []

let build_classes env =
  let _ = pycocci_init () in
  inc_match := true;
  exited := false;
  the_environment := env;
  let mx = !coccinelle_module in
  let dict = pymodule_getdict mx in
  List.iter
    (function
	"include_match" | "has_env_binding" | "exit" -> ()
      | name ->
	  let v = pydict_delitemstring(dict,name) in
	  check_int_return_value ("removing " ^ name ^ " from python coccinelle module") v)
    !added_variables;
  added_variables := [];
  ()

let build_variable name value =
  let mx = !coccinelle_module in
  added_variables := name :: !added_variables;
  check_int_return_value ("build python variable " ^ name)
    (pydict_setitemstring(pymodule_getdict mx, name, value))

let get_variable name =
  let mx = !coccinelle_module in
  pystring_asstring
    (pyobject_str(pydict_getitemstring(pymodule_getdict mx, name)))

let contains_binding e (_,(r,m),_) =
  try
    let _ = List.find (function ((re, rm), _) -> r = re && m = rm) e in
    true
  with Not_found -> false

let construct_variables mv e =
  let find_binding (r,m) =
    try
      let elem = List.find (function ((re,rm),_) -> r = re && m = rm) e in
      Some elem
    with Not_found -> None
  in

(* Only string in this representation, so no point
  let instantiate_Expression(x) =
    let str = pystring_fromstring (Pycocci_aux.exprrep x) in
    pycocci_instantiate_class "coccilib.elems.Expression"
      (pytuple_fromsingle (str))
  in
*)

(* Only string in this representation, so no point
  let instantiate_Identifier(x) =
    let str = pystring_fromstring x in
    pycocci_instantiate_class "coccilib.elems.Identifier"
      (pytuple_fromsingle (str))
  in
*)

  let instantiate_term_list py printer lst  =
    let (str,elements) = printer lst in
    let str = pystring_fromstring str in
    let elements =
      pytuple_fromarray
	(Array.of_list (List.map pystring_fromstring elements)) in
    let repr =
      pycocci_instantiate_class "coccilib.elems.TermList"
	(pytuple_fromarray (Array.of_list [str;elements])) in
    let _ = build_variable py repr in () in

  List.iter (function (py,(r,m),_) ->
    match find_binding (r,m) with
      None -> ()
(*    | Some (_, Ast_c.MetaExprVal (expr,_)) ->
       let expr_repr = instantiate_Expression(expr) in
       let _ = build_variable py expr_repr in
       () *)
  (*  | Some (_, Ast_c.MetaIdVal id) ->
       let id_repr = instantiate_Identifier(id) in
       let _ = build_variable py id_repr in
       () *)
    | Some (_, Ast_c.MetaExprListVal (exprlist)) ->
	instantiate_term_list py Pycocci_aux.exprlistrep exprlist
    | Some (_, Ast_c.MetaParamListVal (paramlist)) ->
	instantiate_term_list py Pycocci_aux.paramlistrep paramlist
    | Some (_, Ast_c.MetaInitListVal (initlist)) ->
	instantiate_term_list py Pycocci_aux.initlistrep initlist
    | Some (_, Ast_c.MetaFieldListVal (fieldlist)) ->
	instantiate_term_list py Pycocci_aux.fieldlistrep fieldlist
    | Some (_, Ast_c.MetaPosValList l) ->
       let locs =
	 List.map
	   (function (fname,current_element,(line,col),(line_end,col_end)) ->
		pycocci_instantiate_class "coccilib.elems.Location"
	       (_pycocci_tuple6
		(pystring_fromstring fname,pystring_fromstring current_element,
		pystring_fromstring (Printf.sprintf "%d" line),
		pystring_fromstring (Printf.sprintf "%d" col),
		pystring_fromstring (Printf.sprintf "%d" line_end),
		pystring_fromstring (Printf.sprintf "%d" col_end)))) l in
       let pylocs = pytuple_fromarray (Array.of_list locs) in
       let _ = build_variable py pylocs in
       ()
    | Some (_,binding) ->
       let _ =
	 build_variable py
	   (pystring_fromstring (Pycocci_aux.stringrep binding)) in
       ()
    ) mv;

  ()

let construct_script_variables mv =
  List.iter
    (function (_,py) ->
      let str =
	pystring_fromstring
	  "initial value: consider using coccinelle.varname" in
      let _ = build_variable py str in
      ())
    mv

let retrieve_script_variables mv =
  List.map (function (_,py) -> Ast_c.MetaIdVal(get_variable py)) mv

let set_coccifile cocci_file =
	cocci_file_name := cocci_file;
	()

let pyrun_simplestring s =
  let res = Pycaml.pyrun_simplestring s in
  check_int_return_value ("running simple python string:\n" ^ s) res;
  res

let py_isinitialized () =
  Pycaml.py_isinitialized ()


let py_finalize () =
  Pycaml.py_finalize ()
