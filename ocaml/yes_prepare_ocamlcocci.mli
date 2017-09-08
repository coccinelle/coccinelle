exception CompileFailure of string
exception LinkFailure of string
val ext : string
val sysdir : unit -> string
val check_cmd : string -> bool
val to_opt : string -> string
val check_runtime : unit -> unit
val init_ocamlcocci : 'a -> string
val print_match : int ref -> string -> string -> string
val string_rep_binding : int ref -> string option * Ast_cocci.metavar -> string
val ast_rep_binding : int ref -> string option * Ast_cocci.metavar -> string
val manage_script_vars : ('a * string) list -> string
val print_iteration_code : out_channel -> unit
val prepare_mvs :
  out_channel ->
  string ->
  ((string option * 'a option) * (string * string) * 'b * Ast_cocci.mvinit) list ->
  bool
val prepare_generic_rule :
  string * ((string option * string option) * 'a * Ast_cocci.metavar * 'b) list *
  ('c * string) list * string -> string -> string -> string
val prepare_rule :
  string * ((string option * string option) * 'a * Ast_cocci.metavar * 'b) list *
  ('c * string) list * string -> string
val prepare_constraint :
  string * (('a * string) * Ast_cocci.metavar) list * string -> string
val prepare : 'a -> Ast_cocci.rule list -> string option
val prepare_simple : string -> string
val find_cmifile : string -> string
module ModuleSet : Set.S with type elt = string
val approx_coccilib_deps : string -> (string, unit) Hashtbl.t
val filter_dep :
  (string, 'a) Hashtbl.t ->
  string list * string list -> string -> string list * string list
val get_dir : string -> string * string
val parse_dep : string -> string -> string -> (string * string) list * string
val dep_flag : string -> string -> (string * string) list * string
val compile_bytecode_cmd : string -> string -> string * string
val compile_native_cmd : string -> string -> string * string
val compile : string -> string -> unit
val load_obj : string -> unit
val load_lib : string * string -> unit
val load_libs : (string * string) list -> unit
val load_file : string -> unit
val clean_file : string -> unit
val test : unit -> unit
