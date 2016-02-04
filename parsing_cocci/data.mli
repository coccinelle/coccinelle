(*
 * Copyright 2012, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./data.mli"
(* types that clutter the .mly file *)
(* for iso metavariables, true if they can only match nonmodified, unitary
   metavariables *)
type fresh = bool

type incl_iso =
    Include of string | Iso of (string,string) Common.either
  | Virt of string list (* virtual rules *)

type clt =
    line_type * int * int * int * int (* starting spaces *) *
      (Ast_cocci.added_string * Ast0_cocci.position_info) list (*code before*) *
      (Ast_cocci.added_string * Ast0_cocci.position_info) list (*code after *) *
      Ast0_cocci.anything list (* position variable, minus only *)

(* ---------------------------------------------------------------------- *)

and line_type =
    MINUS | OPTMINUS | UNIQUEMINUS
  | PLUS | PLUSPLUS
  | CONTEXT | UNIQUE | OPT

type iconstraints = Ast_cocci.idconstraint
type econstraints = Ast0_cocci.constraints
type pconstraints = Ast_cocci.meta_name list

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

val add_meta_meta: (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_id_meta:
    (Ast_cocci.meta_name -> iconstraints -> Ast0_cocci.pure -> unit) ref

val add_virt_id_meta_found: (string -> string -> unit) ref

val add_virt_id_meta_not_found:
    (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_fresh_id_meta: (Ast_cocci.meta_name -> Ast_cocci.seed -> unit) ref

val add_type_meta: (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_init_meta: (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_initlist_meta:
    (Ast_cocci.meta_name -> Ast_cocci.list_len -> Ast0_cocci.pure ->
      unit) ref

val add_param_meta: (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_paramlist_meta:
    (Ast_cocci.meta_name -> Ast_cocci.list_len -> Ast0_cocci.pure ->
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
    (Ast_cocci.meta_name -> Ast_cocci.list_len -> Ast0_cocci.pure ->
      unit) ref

val add_decl_meta: (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_field_meta: (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref

val add_symbol_meta: (string -> unit) ref

val add_field_list_meta:
    (Ast_cocci.meta_name -> Ast_cocci.list_len -> Ast0_cocci.pure -> unit) ref

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

val add_fmt_meta: (Ast_cocci.meta_name -> iconstraints -> unit) ref

val add_fmtlist_meta: (Ast_cocci.meta_name -> Ast_cocci.list_len -> unit) ref

val add_type_name: (string -> unit) ref

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
