(* types that clutter the .mly file *)
(* for iso metavariables, true if they can only match nonmodified, unitary
   metavariables *)
type fresh = bool

type clt =
    line_type * int * int * int * int (* starting spaces *) *
      string list (* code before *) * string list (* code after *) *
      Ast0_cocci.meta_pos (* position variable, minus only *)

(* ---------------------------------------------------------------------- *)

and line_type =
    MINUS | OPTMINUS | UNIQUEMINUS
  | PLUS
  | CONTEXT | UNIQUE | OPT

type iconstraints = Ast0_cocci.ident list
type econstraints = Ast0_cocci.expression list
type pconstraints = Ast_cocci.meta_name list

val in_rule_name : bool ref (* true if parsing the rule name *)
val in_meta : bool ref      (* true if parsing the metavariable decls *)
val in_iso : bool ref       (* true if parsing the isomorphisms *)
val in_prolog : bool ref    (* true if parsing the beginning of an SP *)
val inheritable_positions : string list ref

val all_metadecls : (string, Ast_cocci.metavar list) Hashtbl.t

val clear_meta: (unit -> unit) ref

val add_id_meta:
    (Ast_cocci.meta_name -> iconstraints -> Ast0_cocci.pure -> unit) ref

val add_type_meta: (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_param_meta: (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_paramlist_meta:
    (Ast_cocci.meta_name -> Ast_cocci.meta_name option -> Ast0_cocci.pure ->
      unit) ref

val add_const_meta:
    (Type_cocci.typeC list option -> Ast_cocci.meta_name -> econstraints ->
      Ast0_cocci.pure -> unit) ref

val add_err_meta:
    (Ast_cocci.meta_name -> econstraints -> Ast0_cocci.pure -> unit) ref

val add_exp_meta:
    (Type_cocci.typeC list option -> Ast_cocci.meta_name -> econstraints ->
      Ast0_cocci.pure -> unit) ref

val add_idexp_meta:
    (Type_cocci.typeC list option -> Ast_cocci.meta_name ->
      econstraints -> Ast0_cocci.pure -> unit) ref

val add_local_idexp_meta:
    (Type_cocci.typeC list option -> Ast_cocci.meta_name ->
      econstraints -> Ast0_cocci.pure -> unit) ref

val add_explist_meta:
    (Ast_cocci.meta_name -> Ast_cocci.meta_name option -> Ast0_cocci.pure ->
      unit) ref

val add_stm_meta: (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_stmlist_meta: (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_func_meta:
    (Ast_cocci.meta_name -> iconstraints -> Ast0_cocci.pure -> unit) ref

val add_local_func_meta:
    (Ast_cocci.meta_name -> iconstraints -> Ast0_cocci.pure -> unit) ref

val add_declarer_meta:
    (Ast_cocci.meta_name -> iconstraints -> Ast0_cocci.pure -> unit) ref

val add_iterator_meta:
    (Ast_cocci.meta_name -> iconstraints -> Ast0_cocci.pure -> unit) ref

val add_pos_meta:
    (Ast_cocci.meta_name -> pconstraints -> Ast_cocci.meta_collect -> unit) ref

val add_type_name: (string -> unit) ref

val add_declarer_name: (string -> unit) ref

val add_iterator_name: (string -> unit) ref

val init_rule: (unit -> unit) ref

val install_bindings: (string -> unit) ref
