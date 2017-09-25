(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Ast_c
open Common
open Pycocci_aux
module StringMap = Map.Make (String)

exception Pycocciexception

let python_support = true

let initialised = ref false

let coccinelle_module = ref Py.none
let cocci_file_name = ref ""

(* dealing with python modules loaded *)
let module_map = ref (StringMap.add "__main__" Py.none StringMap.empty)

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
    let m = Py.Import.import_module module_name in
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
  Py.Module.get m class_name

let pycocci_instantiate_class fqn args =
  let class_type = pycocci_get_class_type fqn in
  Py.Eval.call_object class_type args

(* end python interaction *)

let inc_match = ref true
let exited = ref false

let include_match v =
  inc_match := Py.Bool.to_bool (Py.Tuple.get_item v 1);
  Py.none

let sp_exit _ =
  exited := true;
  Py.none

let build_class cname parent fields methods pymodule =
  let cx =
    Py.Class.init (Py.String.of_string cname)
      ~parents:(Py.Tuple.singleton (pycocci_get_class_type parent))
      ~fields ~methods in
  Py.Module.set pymodule cname cx;
  cx

let the_environment = ref []

let has_environment_binding args =
  let (rule, name) = (Py.Tuple.get_item args 1, Py.Tuple.get_item args 2) in
  let orule = Py.String.to_string rule in
  let oname = Py.String.to_string name in
  let e = List.exists (function (x,y) -> orule = x && oname = y)
      !the_environment in
  Py.Bool.of_bool e

let pyoption pyobject =
  if pyobject = Py.none then
    None
  else
    Some pyobject

let string_pair_of_pytuple pytuple =
  let (s0, s1) = Py.Tuple.to_pair pytuple in
  (Py.String.to_string s0, Py.String.to_string s1)

let add_pending_instance args =
  let py_files = Py.Tuple.get_item args 1 in
  let py_virtual_rules = Py.Tuple.get_item args 2 in
  let py_virtual_identifiers = Py.Tuple.get_item args 3 in
  let py_extend_virtual_ids = Py.Tuple.get_item args 4 in
  let files =
    Common.map_option (Py.List.to_list_map Py.String.to_string)
      (pyoption py_files) in
  let virtual_rules =
    Py.List.to_list_map Py.String.to_string py_virtual_rules in
  let virtual_identifiers =
    Py.List.to_list_map string_pair_of_pytuple py_virtual_identifiers in
  let extend_virtual_ids = Py.Bool.to_bool py_extend_virtual_ids in
  Iteration.add_pending_instance
    (files, virtual_rules, virtual_identifiers, extend_virtual_ids);
  Py.none

let pycocci_init_not_called _ = failwith "pycocci_init() not called"

let pywrap_ast = ref pycocci_init_not_called

let pyunwrap_ast = ref pycocci_init_not_called

let wrap_make metavar_of_pystring args =
  let arg = Py.Tuple.get_item args 1 in
  let s = Py.String.to_string arg in
  let mv = metavar_of_pystring s in
  !pywrap_ast mv

let wrap_make_stmt_with_env args =
  let arg_env = Py.Tuple.get_item args 1 in
  let arg_s = Py.Tuple.get_item args 2 in
  let env = Py.String.to_string arg_env in
  let s = Py.String.to_string arg_s in
  let mv = Coccilib.make_stmt_with_env env s in
  !pywrap_ast mv

let wrap_make_listlen args =
  let arg = Py.Tuple.get_item args 1 in
  let i = Py.Long.to_int arg in
  let mv = Coccilib.make_listlen i in
  !pywrap_ast mv

let wrap_make_position args =
  let arg_fl = Py.Tuple.get_item args 1 in
  let arg_fn = Py.Tuple.get_item args 2 in
  let arg_startl = Py.Tuple.get_item args 3 in
  let arg_startc = Py.Tuple.get_item args 4 in
  let arg_endl = Py.Tuple.get_item args 5 in
  let arg_endc = Py.Tuple.get_item args 6 in
  let fl = Py.String.to_string arg_fl in
  let fn = Py.String.to_string arg_fn in
  let startl = Py.Long.to_int arg_startl in
  let startc = Py.Long.to_int arg_startc in
  let endl = Py.Long.to_int arg_endl in
  let endc = Py.Long.to_int arg_endc in
  let mv = Coccilib.make_position fl fn startl startc endl endc in
  !pywrap_ast mv

let pyoutputinstance = ref Py.none

let get_cocci_file args = Py.String.of_string !cocci_file_name

(* initialisation routines *)
let _pycocci_setargs argv0 =
  let argv =
    Py.Sequence.list (Py.Tuple.singleton (Py.String.of_string argv0)) in
  let sys_mod = load_module "sys" in
  Py.Module.set sys_mod "argv" argv

let initialize_python_path () =
  let sep = ":" in
  let python_libdir = Filename.concat Config.path "python" in
  match Common.optionise (fun () -> Sys.getenv "PYTHONPATH") with
    None -> Unix.putenv "PYTHONPATH" python_libdir
  | Some paths ->
      let path_list = Common.split sep paths in
      if not (List.mem python_libdir path_list) then
	Unix.putenv "PYTHONPATH"
	  (Printf.sprintf "%s%s%s" paths sep python_libdir)

let pycocci_init () =
  (* initialize *)
  if not !initialised then (
    initialize_python_path ();
    let _ = if not (Py.is_initialized ()) then
      (if !Flag.show_misc then Common.pr2 "Initializing python\n%!";
	Py.initialize ~interpreter:!Config.python_interpreter ()) in

  (* set argv *)
  let argv0 = Sys.executable_name in
  let _ = _pycocci_setargs argv0 in

  coccinelle_module := Py.Module.create "coccinelle";
  module_map := StringMap.add "coccinelle" !coccinelle_module !module_map;
  let _ = load_module "coccilib.elems" in
  let _ = load_module "coccilib.output" in

  let module_dictionary = Py.Import.get_module_dict () in
  coccinelle_module := Py.Module.create "coccinelle";
  let mx = !coccinelle_module in
  let mypystring = Py.String.of_string !cocci_file_name in
  let cx = build_class "Cocci" (!Flag.pyoutput)
      [("cocci_file", mypystring)]
      [("exit", sp_exit);
	("include_match", include_match);
	("has_env_binding", has_environment_binding);
	("add_pending_instance", add_pending_instance);
	("make_ident", wrap_make Coccilib.make_ident);
	("make_expr", wrap_make Coccilib.make_expr);
	("make_stmt", wrap_make Coccilib.make_stmt);
	("make_stmt_with_env", wrap_make_stmt_with_env);
	("make_type", wrap_make Coccilib.make_type);
	("make_listlen", wrap_make_listlen);
	("make_position", wrap_make_position);
     ] mx in
  pyoutputinstance := cx;
  Py.Dict.set_item_string module_dictionary "coccinelle" mx;

  let (wrap_ast, unwrap_ast) = Py.Capsule.make "metavar_binding_kind" in
  pywrap_ast := wrap_ast;
  pyunwrap_ast := unwrap_ast;
  initialised := true;
  ()) else
  ()

(*let _ = pycocci_init ()*)
(* end initialisation routines *)

let default_hashtbl_size = 17

let added_variables = Hashtbl.create default_hashtbl_size

let catch_python_error f =
  try
    f ()
  with Py.E (_, error) ->
    failwith (Printf.sprintf "Python error: %s" (Py.Object.to_string error))

let build_classes env =
  catch_python_error begin fun () ->
    let _ = pycocci_init () in
    inc_match := true;
    exited := false;
    the_environment := env;
    let mx = !coccinelle_module in
    Hashtbl.iter
      (fun name () ->
	match name with
	  "include_match" | "has_env_binding" | "exit" -> ()
	| name -> Py.Module.remove mx name)
      added_variables;
    Hashtbl.clear added_variables
  end

let build_variable name value =
  let mx = !coccinelle_module in
  Hashtbl.replace added_variables name ();
  Py.Module.set mx name value

let get_variable name =
  let mx = !coccinelle_module in
  Py.Module.get mx name

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
    let str = Py.String.of_string str in
    let elements = Py.Tuple.of_list_map Py.String.of_string elements in
    let repr =
      pycocci_instantiate_class "coccilib.elems.TermList"
	(Py.Tuple.of_pair (str, elements)) in
    let _ = build_variable py repr in () in

  List.iter (function (py,(r,m),_,init) ->
    match find_binding (r,m) with
      None ->
	(match init with
	  Ast_cocci.MVInitString s ->
            let _ = build_variable py (Py.String.of_string s) in
	    ()
	| Ast_cocci.MVInitPosList ->
	    let pylocs = Py.Tuple.create 0 in
	    let _ = build_variable py pylocs in
	    ()
	| Ast_cocci.NoMVInit ->
	    failwith "python variables should be bound")
(*    | Some (_, Ast_c.MetaExprVal (expr,_,_)) ->
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
	   (function (fname,current_element,current_element_pos,
		      (line,col),(line_end,col_end)) ->
	     let (current_element_line,current_element_col,
		  current_element_line_end,current_element_col_end) =
	       match current_element_pos with
		 Some
		   ((current_element_line,current_element_col),
		    (current_element_line_end,current_element_col_end)) ->
		      (current_element_line,current_element_col,
		       current_element_line_end,current_element_col_end)
	       | None -> (-1,-1,-1,-1) in
	     pycocci_instantiate_class "coccilib.elems.Location"
	       (Py.Tuple.of_list_map Py.String.of_string
		  [fname;
		   current_element;
		   string_of_int current_element_line;
		   string_of_int current_element_col;
		   string_of_int current_element_line_end;
		   string_of_int current_element_col_end;
		   string_of_int line;
		   string_of_int col;
		   string_of_int line_end;
		   string_of_int col_end])) l in
       let pylocs = Py.Tuple.of_list locs in
       let _ = build_variable py pylocs in
       ()
    | Some (_,binding) ->
       let _ =
	 build_variable py
	   (Py.String.of_string (Pycocci_aux.stringrep binding)) in
       ()
    ) mv;

  let add_string_literal s = build_variable s (Py.String.of_string s) in
  List.iter add_string_literal !Iteration.parsed_virtual_rules;
  List.iter add_string_literal !Iteration.parsed_virtual_identifiers

let construct_script_variables mv =
  List.iter
    (function (_,py) ->
      let str =
	Py.String.of_string
	  "initial value: consider using coccinelle.varname" in
      let _ = build_variable py str in
      ())
    mv

let retrieve_script_variables mv =
  let unwrap (_, py) =
    let mx = !coccinelle_module in
    let v = Py.Module.get mx py in
    if Py.String.check v then
      Ast_c.MetaIdVal(Py.String.to_string v)
    else
      !pyunwrap_ast v in
  List.map unwrap mv

let set_coccifile cocci_file =
	cocci_file_name := cocci_file;
	()

let pickle_variable var =
  Py.Marshal.dumps (get_variable var)

let unpickle_variable var value =
  let py_list = Py.List.of_list (List.map Py.Marshal.loads value) in
  Py.Module.set !coccinelle_module var py_list

let pyrun_simplestring s =
  catch_python_error begin fun () ->
    if not (Py.Run.simple_string s) then
      failwith "Python failure"
  end

let run (file, line) s =
  try
    pyrun_simplestring s
  with Failure msg ->
    failwith (
    Printf.sprintf "Error in Python script, line %d, file \"%s\": %s"
      line file msg)

let py_isinitialized () =
  Py.is_initialized ()

let py_finalize () =
  Py.finalize ()

let run_constraint args pos body =
  catch_python_error begin fun () ->
    build_classes [];
    let make_arg (name, value) =
      (snd name, name, value, Ast_cocci.NoMVInit) in
    let mv = List.map make_arg args in
    construct_variables mv args;
    run pos (Printf.sprintf "
from coccinelle import *
from coccilib.iteration import Iteration

coccinelle.result = (%s)" body);
    Py.Bool.to_bool (get_variable "result")
  end

let flush_stdout_and_stderr () =
  if py_isinitialized () then
    let _ = pyrun_simplestring "\
import sys
sys.stdout.flush()
sys.stderr.flush()
" in
    ()
