type line_type =
    MINUS | OPTMINUS | UNIQUEMINUS | MULTIMINUS
  | PLUS
  | CONTEXT | UNIQUE | OPT | MULTI

val in_meta : bool ref (* true if parsing the metavariable decls *)
val in_iso : bool ref  (* true if parsing the isomorphisms *)

val clear_meta: (unit -> unit) ref

val add_id_meta: (string -> bool -> unit) ref

val add_text_meta: (string -> bool -> unit) ref

val add_type_meta: (string -> bool -> unit) ref

val add_param_meta: (string -> bool -> unit) ref

val add_paramlist_meta: (string -> bool -> unit) ref

val add_const_meta:
    (Type_cocci.typeC list option -> string -> bool -> unit) ref

val add_err_meta: (string -> bool -> unit) ref

val add_exp_meta:
    (Type_cocci.typeC list option -> string -> bool -> unit) ref

val add_explist_meta: (string -> bool -> unit) ref

val add_stm_meta: (string -> bool -> unit) ref

val add_stmlist_meta: (string -> bool -> unit) ref

val add_func_meta: (string -> bool -> unit) ref

val add_local_func_meta: (string -> bool -> unit) ref

val add_type_name: (string -> unit) ref

(* ---------------------------------------------------------------------- *)

(* types that clutter the .mly file *)
(* for iso metavariables, true if they can only match nonmodified, unitary
   metavariables *)
type pure = bool

type clt = line_type * int * int * int
