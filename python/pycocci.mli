(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val build_classes : Ast_cocci.meta_name list -> unit
val construct_variables :
    (string * Ast_cocci.meta_name * Ast_cocci.metavar * Ast_cocci.mvinit) list
  -> Ast_c.metavars_binding (*virts*) -> unit
val construct_script_variables : Ast_cocci.meta_name list -> unit
val pyrun_simplestring : string -> unit
val inc_match : bool ref
val exited : bool ref
val retrieve_script_variables :
    Ast_cocci.meta_name list -> Ast_c.metavar_binding_kind list
exception Pycocciexception
val set_coccifile : string -> unit
val python_support : bool
val initialised : bool ref
val py_isinitialized : unit -> bool
val py_finalize : unit -> unit

val run_constraint :
    Ast_c.metavars_binding ->
    string ->
    bool
(** [run_constraint args body] runs the constraint with
    @param args the arguments
    @param body the constraint code,
    @return whether the constraint is satisfied or not. *)
