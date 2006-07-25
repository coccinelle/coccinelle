type line_type =
    MINUS | OPTMINUS | UNIQUEMINUS | MULTIMINUS
  | PLUS
  | CONTEXT | UNIQUE | OPT | MULTI

val clear_meta: (unit -> unit) ref

val add_id_meta: (string -> unit) ref

val add_type_meta: (string -> unit) ref

val add_param_meta: (string -> unit) ref

val add_paramlist_meta: (string -> unit) ref

val add_const_meta: (Type_cocci.typeC list option -> string -> unit) ref

val add_err_meta: (string -> unit) ref

val add_exp_meta: (Type_cocci.typeC list option -> string -> unit) ref

val add_explist_meta: (string -> unit) ref

val add_stm_meta: (string -> unit) ref

val add_stmlist_meta: (string -> unit) ref

val add_func_meta: (string -> unit) ref

val add_local_func_meta: (string -> unit) ref
