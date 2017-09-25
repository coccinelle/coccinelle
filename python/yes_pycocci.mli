module StringMap : Map.S with type key = string
exception Pycocciexception
val python_support : bool
val initialised : bool ref
val coccinelle_module : Py.Object.t ref
val cocci_file_name : string ref
val module_map : Py.Object.t StringMap.t ref
val get_module : StringMap.key -> Py.Object.t
val is_module_loaded : StringMap.key -> bool
val load_module : StringMap.key -> Py.Object.t
val split_fqn : string -> string * string
val pycocci_get_class_type : string -> Py.Object.t
val pycocci_instantiate_class : string -> Py.Object.t -> Py.Object.t
val inc_match : bool ref
val exited : bool ref
val include_match : Py.Object.t -> Py.Object.t
val sp_exit : 'a -> Py.Object.t
val build_class :
  string ->
  string ->
  (string * Py.Object.t) list ->
  (string * (Py.Object.t -> Py.Object.t)) list -> Py.Object.t -> Py.Object.t
val the_environment : (string * string) list ref
val has_environment_binding : Py.Object.t -> Py.Object.t
val pyoption : Py.Object.t -> Py.Object.t option
val string_pair_of_pytuple : Py.Object.t -> string * string
val add_pending_instance : Py.Object.t -> Py.Object.t
val pycocci_init_not_called : 'a -> 'b
val pywrap_ast : (Coccilib.Ast_c.metavar_binding_kind -> Py.Object.t) ref
val pyunwrap_ast : (Py.Object.t -> Coccilib.Ast_c.metavar_binding_kind) ref
val wrap_make :
  (string -> Coccilib.Ast_c.metavar_binding_kind) ->
  Py.Object.t -> Py.Object.t
val wrap_make_stmt_with_env : Py.Object.t -> Py.Object.t
val wrap_make_listlen : Py.Object.t -> Py.Object.t
val wrap_make_position : Py.Object.t -> Py.Object.t
val pyoutputinstance : Py.Object.t ref
val get_cocci_file : 'a -> Py.Object.t
val _pycocci_setargs : string -> unit
val initialize_python_path : unit -> unit
val pycocci_init : unit -> unit
val default_hashtbl_size : int
val added_variables : (string, unit) Hashtbl.t
val catch_python_error : (unit -> 'a) -> 'a
val build_classes : (string * string) list -> unit
val build_variable : string -> Py.Object.t -> unit
val get_variable : string -> Py.Object.t
val contains_binding : (('a * 'b) * 'c) list -> 'd * ('a * 'b) * 'e -> bool
val construct_variables :
  (string * ('a * 'b) * 'c * Ast_cocci.mvinit) list ->
  (('a * 'b) * Ast_c.metavar_binding_kind) list -> unit
val construct_script_variables : ('a * string) list -> unit
val retrieve_script_variables :
  ('a * string) list -> Coccilib.Ast_c.metavar_binding_kind list
val set_coccifile : string -> unit
val pickle_variable : string -> string
val unpickle_variable : string -> string list -> unit
val pyrun_simplestring : string -> unit
val run : string * int -> string -> unit
val py_isinitialized : unit -> bool
val py_finalize : unit -> unit
val run_constraint :
  (('a * string) * Ast_c.metavar_binding_kind) list ->
  string * int -> string -> bool
val flush_stdout_and_stderr : unit -> unit
