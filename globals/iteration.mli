type init_info = (string (* language *) * string (* rule name *)) *
      string list (* defined virtual rules *)

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
    (* input is like pending_info, but with an extra option on files *)
    (string list option * string list * (string * string) list) -> unit

val get_pending_instance : unit -> pending_info option

(* ----------------------------------------------------------------------- *)

val check_virtual_rule : string -> unit
val check_virtual_ident : string -> unit
