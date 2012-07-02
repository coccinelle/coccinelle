#use "topfind";;
#require "pycaml";;
#require "snippets";;

open Pycaml;;

(* See http://docs.python.org/lib/module-logging.html for Level values *)
type logging_levels = Notset | Debug | Info | Warn | Error | Critical | Level of int;;

(* Set default logger *)
let default_logger = (fun l m -> Printf.printf "ocaml-default log: %d %s%!\n%!" l m);;

let loggers = Hashtbl.create 10;;
Hashtbl.add loggers "default-ocaml" default_logger;;

(* Translate level names into numerical values as used by Python 
   See http://docs.python.org/lib/module-logging.html for Level values *)
let int_of_level name =
  match name with
    | Notset   -> 0
    | Debug    -> 10
    | Info     -> 20 
    | Warn     -> 30
    | Error    -> 40
    | Critical -> 50
    | Level (n)-> n
;;

let find_logger name =
  try
    Hashtbl.find loggers name
  with
    | Not_found -> 
	try 
	  Hashtbl.find loggers "default-ocaml"
	with 
	  | Not_found -> failwith "Default logger missing -- internal problem";;


(* Provide interface functions as in Python's logging module *)
let debug name msg =
  let log = find_logger name in
    log (int_of_level Debug) msg;;

let info name msg =
  let log = find_logger name in
    log (int_of_level Info) msg;;

let warn name msg =
  let log = find_logger name in
    log (int_of_level Warn) msg;;

let warning = warn;;

let error name msg =
  let log = find_logger name in
    log (int_of_level Error) msg;;

let critical name msg =
  let log = find_logger name  in
    log (int_of_level Critical) msg;;

(* Function that takes a name, level integer (this is 'raw') and the message *)
let lograw name level msg =
  let logf = find_logger name in
    logf level msg;;

(* Function that takes a name, level and the message.
   Presumably, this is what will be used most often. *)
let log name level msg =
  let lograw = find_logger name in
    lograw (int_of_level level) msg;;


(* Allowing to register loggers from Python *)

let add_logger_if_new name logger = 
  let loggername = 
    try
      let _ = Str.search_forward (Str.regexp "ocaml") name 0 in
	name
    with 
      | Not_found -> Printf.sprintf "%s-ocaml" name
  in
  try
    let _ = Hashtbl.find loggers loggername
    in failwith (Printf.sprintf "Trying to register logger '%s' again. Why?" loggername)
  with
    | Not_found -> Printf.printf "Adding logger %s to hashtable\n%!" loggername;
	Hashtbl.add loggers loggername logger;;

let _py_register_logger =
  python_pre_interfaced_function
    ~doc:"Register a python-logger. \nArguments: Logger name (str) and callback function cb(). \nThe signature of cb() is cb( level: int, message:str). "
    [|StringType;CallableType|]
    (fun py_args ->
       let name = pystring_asstring py_args.(0) in
       let ocamllogger = (fun level msg -> 
			    let callback_args =
			      pytuple_fromarray
				[|pyint_fromint level;
				  pystring_fromstring msg|]
			    in
			    let _ = pyeval_callobject(py_args.(1),callback_args) in ()
			 )
       in let () = add_logger_if_new name ocamllogger 
     in pynone() );;





(* Debugging tools: *)

(* getinfo returns an array of strings that contain the names of registered loggers. *)
let getinfo loggers =
    let loggerarray = Array.make (Hashtbl.length loggers) "empty" in
    let () = Snippets.hashtbl_iteri (fun i key value -> (loggerarray.(i) <- key)) loggers in
    loggerarray;;

let print_loggers loggers = 
  Array.iter (fun a -> Printf.printf "registered loggername=%s\n%!" a)  (getinfo loggers);;
      
let _py_ocaml_log  =
  python_pre_interfaced_function
    ~doc: "Function that calls the ocaml logger from Python (just for debugging useful) "
    [|StringType;IntType;StringType|] ( fun py_args -> 
					  let name = pystring_asstring py_args.(0) in
					  let level = pyint_asint py_args.(1) in
					  let msg = pystring_asstring py_args.(2) in
					  let () =  lograw name level msg in
					    pynone() );;

(* Register function for Python *)

let () = register_pre_functions_for_python
  [|  ("log", _py_ocaml_log);
      ("register_logger", _py_register_logger) |]
;;



(* This needs to go into the documentation *)

Printf.printf "About to start tests (Ocaml)\n";;

let () = add_logger_if_new "Ocaml" (fun l m -> Printf.printf "Ocamllog, Lev=%d: %s\n" l m);  

python_load "log.py";; 

log "default" (Level 30) "Purely called from Ocaml";;

info "ocaml" "This is information";;

print_loggers loggers;;

python_eval "logger.warn('Warning from Python')";; 

let nmeshlog = log 
(* ipython();; *)

