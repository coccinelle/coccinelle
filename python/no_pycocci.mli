module StringMap :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
exception Pycocciexception
val errmsg : string
val python_support : bool
val check_return_value : 'a -> 'b
val check_int_return_value : 'a -> 'b
val initialised : bool ref
val cocci_file_name : string ref
val get_module : 'a -> 'b
val is_module_loaded : 'a -> 'b
val load_module : 'a -> 'b
val pycocci_init : unit -> unit
val split_fqn : 'a -> 'b
val pycocci_get_class_type : 'a -> 'b
val pycocci_instantiate_class : 'a -> 'b -> 'c
val inc_match : bool ref
val exited : bool ref
val include_match : 'a -> 'b
val sp_exit : 'a -> 'b
val build_method : 'a * 'b * 'c -> 'd -> 'e -> 'f -> 'g
val build_class : 'a -> 'b -> 'c -> 'd -> 'e
val has_environment_binding : 'a -> 'b -> 'c
val get_cocci_file : 'a -> 'b
val build_classes : 'a -> 'b
val build_variable : 'a -> 'b -> 'c
val contains_binding : 'a -> 'b * ('c * 'd) * 'e -> 'f
val construct_variables : 'a -> 'b -> 'c
val construct_script_variables : 'a -> 'b
val retrieve_script_variables : 'a -> 'b
val set_coccifile : string -> unit
val pickle_variable : 'a -> 'b
val unpickle_variable : 'a -> 'b -> 'c
val pyrun_simplestring : 'a -> 'b
val run : 'a -> 'b
val py_isinitialized : unit -> 'a
val py_finalize : unit -> 'a
val run_constraint : 'a -> 'b -> 'c -> 'd
val run_fresh_id : 'a -> 'b -> 'c -> 'd
val flush_stdout_and_stderr : unit -> unit
