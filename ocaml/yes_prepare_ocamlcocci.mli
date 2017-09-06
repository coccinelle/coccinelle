module Ast = Ast_cocci
exception CompileFailure of string
exception LinkFailure of string
val ext : string
val sysdir : unit -> string
val check_cmd : string -> bool
val to_opt : string -> string
val check_runtime : unit -> unit
val init_ocamlcocci : 'a -> string
val print_match : int ref -> string -> string -> string
val string_rep_binding : int ref -> string option * Ast.metavar -> string
val ast_rep_binding : int ref -> string option * Ast.metavar -> string
val manage_script_vars : ('a * string) list -> string
val print_iteration_code : out_channel -> unit
val prepare_mvs :
  out_channel ->
  string ->
  ((string option * 'a option) * (string * string) * 'b * Ast.mvinit) list ->
  bool
val prepare_generic_rule :
  string * ((string option * string option) * 'a * Ast.metavar * 'b) list *
  ('c * string) list * string -> string -> string -> string
val prepare_rule :
  string * ((string option * string option) * 'a * Ast.metavar * 'b) list *
  ('c * string) list * string -> string
val prepare_constraint :
  string * (('a * string) * Ast.metavar) list * string -> string
val prepare : 'a -> Ast_cocci.rule list -> string option
val prepare_simple : string -> string
val find_cmifile : string -> string
module ModuleSet :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end
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
