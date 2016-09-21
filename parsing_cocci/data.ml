(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* types that clutter the .mly file *)
(* for iso metavariables, true if they can only match nonmodified, unitary
   metavariables *)
type fresh = bool

type incl_iso =
    Include of string | Iso of (string,string) Common.either
  | Virt of string list (* virtual rules *)

type clt =
    line_type * int * int * int * int * int (* starting spaces *) *
      (Ast_cocci.added_string * Ast0.position_info) list (* code before *) *
      (Ast_cocci.added_string * Ast0.position_info) list (* code after *) *
      Ast0.anything list (* position variable, minus only *) *
      string (* whitespace before *)

(* ---------------------------------------------------------------------- *)

(* Things that need to be seen by the lexer and parser. *)

and line_type =
    MINUS | OPTMINUS
  | PLUS | PLUSPLUS
  | CONTEXT | OPT

type iconstraints = Ast.general_constraint
type econstraints = Ast0.constraints
type pconstraints = Ast.general_constraint

let in_rule_name = ref false
let in_meta = ref false
let in_iso = ref false
let in_generating = ref false
let ignore_patch_or_match = ref false
let in_prolog = ref false
(* state machine for lexer..., allows smpl keywords as type names *)
let saw_struct = ref false
let inheritable_positions =
  ref ([] : string list) (* rules from which posns can be inherited *)

let call_in_meta f =
  in_meta := true; saw_struct := false;
  let res = f() in
  in_meta := false;
  res

let all_metadecls =
  (Hashtbl.create(100) : (string, Ast.metavar list) Hashtbl.t)

let uninitialized_add_meta = fun _ -> failwith "uninitialized add_meta"

let clear_meta: (unit -> unit) ref = ref uninitialized_add_meta

type meta_type = Ast.meta_name -> Ast0.pure -> unit
type id_meta_type = Ast.meta_name -> iconstraints -> Ast0.pure -> unit
type list_meta_type = Ast.meta_name -> Ast.list_len -> Ast0.pure -> unit
type exp_meta_type =
    Ast0.typeC list option -> Ast.meta_name -> econstraints ->
      Ast0.pure -> unit

let add_meta_meta: meta_type ref = ref uninitialized_add_meta
let add_id_meta: id_meta_type ref = ref uninitialized_add_meta

let add_virt_id_meta_found: (string -> string -> unit) ref =
  ref uninitialized_add_meta

let add_virt_id_meta_not_found: meta_type ref = ref uninitialized_add_meta

let add_fresh_id_meta: (Ast.meta_name -> Ast.seed -> unit) ref =
  ref uninitialized_add_meta

let add_type_meta: meta_type ref = ref uninitialized_add_meta
let add_init_meta: meta_type ref = ref uninitialized_add_meta
let add_initlist_meta: list_meta_type ref = ref uninitialized_add_meta
let add_param_meta: meta_type ref = ref uninitialized_add_meta
let add_paramlist_meta: list_meta_type ref = ref uninitialized_add_meta
let add_const_meta: exp_meta_type ref = ref uninitialized_add_meta

let add_err_meta:
    (Ast.meta_name -> econstraints -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_exp_meta: exp_meta_type ref = ref uninitialized_add_meta
let add_idexp_meta: exp_meta_type ref = ref uninitialized_add_meta
let add_local_idexp_meta: exp_meta_type ref = ref uninitialized_add_meta
let add_global_idexp_meta: exp_meta_type ref = ref uninitialized_add_meta
let add_explist_meta: list_meta_type ref = ref uninitialized_add_meta
let add_decl_meta: meta_type ref = ref uninitialized_add_meta
let add_field_meta: meta_type ref = ref uninitialized_add_meta
let add_field_list_meta: list_meta_type ref = ref uninitialized_add_meta
let add_symbol_meta: (string -> unit) ref = ref uninitialized_add_meta
let add_stm_meta: meta_type ref = ref uninitialized_add_meta
let add_stmlist_meta: list_meta_type ref = ref uninitialized_add_meta
let add_dparamlist_meta: list_meta_type ref = ref uninitialized_add_meta
let add_func_meta: id_meta_type ref = ref uninitialized_add_meta
let add_local_func_meta: id_meta_type ref = ref uninitialized_add_meta
let add_declarer_meta: id_meta_type ref = ref uninitialized_add_meta
let add_iterator_meta: id_meta_type ref = ref uninitialized_add_meta

let add_pos_meta:
    (Ast.meta_name -> pconstraints -> Ast.meta_collect -> unit) ref =
  ref uninitialized_add_meta

let add_fmt_meta: (Ast.meta_name -> iconstraints -> unit) ref =
  ref uninitialized_add_meta

let add_fmtlist_meta: (Ast.meta_name -> Ast.list_len -> unit) ref =
  ref uninitialized_add_meta

let add_assignOp_meta:
    (Ast.meta_name -> Ast.general_constraint -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_binaryOp_meta:
    (Ast.meta_name -> Ast.general_constraint -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_type_name: (string -> unit) ref = ref uninitialized_add_meta
let add_attribute: (string -> unit) ref = ref uninitialized_add_meta
let add_declarer_name: (string -> unit) ref = ref uninitialized_add_meta
let add_iterator_name: (string -> unit) ref = ref uninitialized_add_meta

let uninitialized_install_bindings =
  fun _ -> failwith "uninitialized install_bindings"

let init_rule: (unit -> unit) ref = ref uninitialized_install_bindings
let install_bindings: (string -> unit) ref = ref uninitialized_install_bindings

(* ---------------------------------------------------------------------- *)
(* String format things *)

let format_metavariables      =
  ref ([] : (string * (Ast.meta_name * iconstraints)) list)
let format_list_metavariables =
  ref ([] : (string * (Ast.meta_name * Ast.list_len)) list)
