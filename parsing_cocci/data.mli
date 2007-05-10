type line_type =
    MINUS | OPTMINUS | UNIQUEMINUS | MULTIMINUS
  | PLUS
  | CONTEXT | UNIQUE | OPT | MULTI

val in_rule_name : bool ref (* true if parsing the rule name *)
val in_meta : bool ref (* true if parsing the metavariable decls *)
val in_iso : bool ref  (* true if parsing the isomorphisms *)

val all_metadecls : (string, Ast_cocci.metavar list) Hashtbl.t

val clear_meta: (unit -> unit) ref

val add_id_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_text_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_type_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_param_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_paramlist_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_const_meta:
    (Type_cocci.typeC list option -> (string * string) -> Ast0_cocci.pure -> unit) ref

val add_err_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_exp_meta:
    (Type_cocci.typeC list option -> (string * string) -> Ast0_cocci.pure -> unit) ref

val add_explist_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_stm_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_stmlist_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_func_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_local_func_meta: ((string * string) -> Ast0_cocci.pure -> unit) ref

val add_type_name: (string -> unit) ref

val add_declarer_name: (string -> unit) ref

val init_rule: (unit -> unit) ref

val install_bindings: (string -> unit) ref

(* ---------------------------------------------------------------------- *)

(* types that clutter the .mly file *)
(* for iso metavariables, true if they can only match nonmodified, unitary
   metavariables *)
type fresh = bool

type clt =
    line_type * int * int * int * int (* starting spaces *) *
      string list (* code before *) * string list (* code after *)
