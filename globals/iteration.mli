(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

type init_info = (string (* language *) * string (* rule name *)) *
      (string list (* defined virtual rules *) *
	 (string * string) list (* defined virtual env *))

val initialization_stack : init_info list ref

(* ----------------------------------------------------------------------- *)

val base_file_list : string list ref
val parsed_virtual_rules : string list ref
val parsed_virtual_identifiers : string list ref

(* ----------------------------------------------------------------------- *)

type pending_info = string list (* files to treat *) *
      string list * (* defined virtual rules *)
      (string * string) list (* virtual identifiers *)

val add_pending_instance :
    (* input is like pending_info, but with an extra option on files and bool
    for environment extension *)
    (string list option * string list * (string * string) list * bool) ->
    unit

val get_pending_instance : unit -> pending_info option

(* ----------------------------------------------------------------------- *)

val check_virtual_rule : string -> unit
val check_virtual_ident : string -> unit
