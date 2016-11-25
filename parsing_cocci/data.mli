(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* types that clutter the .mly file *)
(* for iso metavariables, true if they can only match nonmodified, unitary
   metavariables *)
type fresh = bool

type incl_iso =
    Include of string | Iso of (string,string) Common.either
  | Virt of string list (* virtual rules *)

type clt =
    line_type * int * int * int * int * int (* starting spaces *) *
     (Ast_cocci.added_string * Ast0_cocci.position_info) list (*code before*) *
     (Ast_cocci.added_string * Ast0_cocci.position_info) list (*code after *) *
     Ast0_cocci.anything list (* position variable, minus only *) *
     string (* whitespace before *)

(* ---------------------------------------------------------------------- *)

and line_type =
    MINUS | OPTMINUS
  | PLUS | PLUSPLUS
  | CONTEXT | OPT

type iconstraints = Ast_cocci.general_constraint
type econstraints = Ast0_cocci.constraints
type pconstraints = Ast_cocci.general_constraint

val in_rule_name : bool ref (* true if parsing the rule name *)
val in_meta : bool ref      (* true if parsing the metavariable decls *)
val in_iso : bool ref       (* true if parsing the isomorphisms *)
val in_generating : bool ref(* true if generating a rule *)
val ignore_patch_or_match : bool ref (* skip rules not satisfying virt *)
val in_prolog : bool ref    (* true if parsing the beginning of an SP *)
val saw_struct : bool ref   (* true if saw struct/union *)
val inheritable_positions : string list ref

val call_in_meta : (unit -> 'a) -> 'a

val all_metadecls : (string, Ast_cocci.metavar list) Hashtbl.t

val clear_meta: (unit -> unit) ref

type meta_type = Ast_cocci.meta_name -> Ast0_cocci.pure -> unit
type id_meta_type =
    Ast_cocci.meta_name -> iconstraints -> Ast0_cocci.pure -> unit
type list_meta_type =
    Ast_cocci.meta_name -> Ast_cocci.list_len -> Ast0_cocci.pure -> unit
type exp_meta_type =
    Ast0_cocci.typeC list option -> Ast_cocci.meta_name -> econstraints ->
      Ast0_cocci.pure -> unit

val add_meta_meta: meta_type ref
val add_id_meta: id_meta_type ref
val add_virt_id_meta_found: (string -> string -> unit) ref
val add_virt_id_meta_not_found: meta_type ref
val add_fresh_id_meta: (Ast_cocci.meta_name -> Ast_cocci.seed -> unit) ref
val add_type_meta: id_meta_type ref
val add_init_meta: meta_type ref
val add_initlist_meta: list_meta_type ref
val add_param_meta: meta_type ref
val add_paramlist_meta: list_meta_type ref
val add_const_meta: exp_meta_type ref

val add_err_meta:
    (Ast_cocci.meta_name -> econstraints -> Ast0_cocci.pure -> unit) ref

val add_exp_meta: exp_meta_type ref
val add_idexp_meta: exp_meta_type ref
val add_local_idexp_meta: exp_meta_type ref
val add_global_idexp_meta: exp_meta_type ref
val add_explist_meta: list_meta_type ref
val add_decl_meta: meta_type ref
val add_field_meta: meta_type ref
val add_symbol_meta: (string -> unit) ref
val add_field_list_meta: list_meta_type ref
val add_stm_meta: meta_type ref
val add_stmlist_meta: list_meta_type ref
val add_dparamlist_meta: list_meta_type ref
val add_func_meta: id_meta_type ref
val add_local_func_meta: id_meta_type ref
val add_declarer_meta: id_meta_type ref
val add_iterator_meta: id_meta_type ref

val add_pos_meta:
    (Ast_cocci.meta_name -> pconstraints -> Ast_cocci.meta_collect -> unit) ref

val add_fmt_meta: (Ast_cocci.meta_name -> iconstraints -> unit) ref

val add_fmtlist_meta: (Ast_cocci.meta_name -> Ast_cocci.list_len -> unit) ref

val add_assignOp_meta:
    (Ast_cocci.meta_name ->
      Ast_cocci.general_constraint -> Ast0_cocci.pure -> unit) ref

val add_binaryOp_meta:
    (Ast_cocci.meta_name ->
      Ast_cocci.general_constraint -> Ast0_cocci.pure -> unit) ref

val add_type_name: (string -> unit) ref
val add_attribute: (string -> unit) ref
val add_declarer_name: (string -> unit) ref
val add_iterator_name: (string -> unit) ref
val init_rule: (unit -> unit) ref
val install_bindings: (string -> unit) ref

(* ---------------------------------------------------------------------- *)
(* String format things *)

val format_metavariables :
    (string * (Ast_cocci.meta_name * iconstraints)) list ref
val format_list_metavariables :
    (string * (Ast_cocci.meta_name * Ast_cocci.list_len)) list ref
