val build_classes : Ast_cocci.meta_name list -> unit
val construct_variables :
    (string * Ast_cocci.meta_name * Ast_cocci.metavar) list
  -> Ast_c.metavars_binding (*virts*) -> unit
val construct_script_variables : Ast_cocci.meta_name list -> unit
val pyrun_simplestring : string -> int
val inc_match : bool ref
val exited : bool ref
val retrieve_script_variables :
    Ast_cocci.meta_name list -> Ast_c.metavar_binding_kind list
exception Pycocciexception 
val set_coccifile : string -> unit
val python_support : bool
val initialised : bool ref
val py_isinitialized : unit -> int
val py_finalize : unit -> unit 
