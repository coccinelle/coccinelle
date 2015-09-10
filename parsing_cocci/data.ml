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
    MINUS | OPTMINUS | UNIQUEMINUS
  | PLUS | PLUSPLUS
  | CONTEXT | UNIQUE | OPT

type iconstraints = Ast.idconstraint
type econstraints = Ast0.constraints
type pconstraints = Ast.meta_name list

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

let clear_meta: (unit -> unit) ref =
  ref uninitialized_add_meta

let add_meta_meta: (Ast.meta_name -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_id_meta:
    (Ast.meta_name -> iconstraints -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_virt_id_meta_found: (string -> string -> unit) ref =
  ref uninitialized_add_meta

let add_virt_id_meta_not_found:
    (Ast_cocci.meta_name -> Ast0_cocci.pure -> unit) ref =
  ref uninitialized_add_meta

let add_fresh_id_meta: (Ast.meta_name -> Ast.seed -> unit) ref =
  ref uninitialized_add_meta

let add_type_meta: (Ast.meta_name -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_init_meta: (Ast.meta_name -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_initlist_meta:
    (Ast.meta_name -> Ast.list_len -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_param_meta: (Ast.meta_name -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_paramlist_meta:
    (Ast.meta_name -> Ast.list_len -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_const_meta:
    (Type_cocci.typeC list option -> Ast.meta_name -> econstraints ->
      Ast0.pure -> unit)
    ref =
  ref uninitialized_add_meta

let add_err_meta:
    (Ast.meta_name -> econstraints -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_exp_meta:
    (Type_cocci.typeC list option -> Ast.meta_name -> econstraints ->
      Ast0.pure -> unit)
    ref =
  ref uninitialized_add_meta

let add_idexp_meta:
    (Type_cocci.typeC list option -> Ast.meta_name -> econstraints ->
      Ast0.pure -> unit)
    ref =
  ref uninitialized_add_meta

let add_local_idexp_meta:
    (Type_cocci.typeC list option -> Ast.meta_name -> econstraints ->
      Ast0.pure -> unit)
    ref =
  ref uninitialized_add_meta

let add_global_idexp_meta:
    (Type_cocci.typeC list option -> Ast.meta_name -> econstraints ->
      Ast0.pure -> unit)
    ref =
  ref (fun _ -> failwith "uninitialized add_meta")

let add_explist_meta:
    (Ast.meta_name -> Ast.list_len -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_decl_meta: (Ast.meta_name -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_field_meta: (Ast.meta_name -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_field_list_meta:
    (Ast.meta_name -> Ast.list_len -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_symbol_meta: (string -> unit) ref =
  ref uninitialized_add_meta

let add_stm_meta: (Ast.meta_name -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_stmlist_meta: (Ast.meta_name -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_func_meta:
    (Ast.meta_name -> iconstraints -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_local_func_meta:
    (Ast.meta_name -> iconstraints -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_declarer_meta:
    (Ast.meta_name -> iconstraints -> Ast0.pure -> unit) ref =
  ref (fun _ -> failwith "uninitialized add_decl")

let add_iterator_meta:
    (Ast.meta_name -> iconstraints -> Ast0.pure -> unit) ref =
  ref (fun _ -> failwith "uninitialized add_iter")

let add_pos_meta:
    (Ast.meta_name -> pconstraints -> Ast.meta_collect -> unit) ref =
  ref uninitialized_add_meta

let add_fmt_meta: (Ast.meta_name -> iconstraints -> unit) ref =
  ref uninitialized_add_meta

let add_fmtlist_meta:
    (Ast.meta_name -> Ast.list_len -> unit) ref =
  ref uninitialized_add_meta

let add_assignOp_meta:
    (Ast.meta_name ->
      Ast0.assignOpconstraint -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_binaryOp_meta:
    (Ast.meta_name ->
      Ast0.binaryOpconstraint -> Ast0.pure -> unit) ref =
  ref uninitialized_add_meta

let add_type_name: (string -> unit) ref =
  ref (fun _ -> failwith "uninitialized add_type")

let add_attribute: (string -> unit) ref =
  ref (fun _ -> failwith "uninitialized add_attribute")

let add_declarer_name: (string -> unit) ref =
  ref (fun _ -> failwith "uninitialized add_decl")

let add_iterator_name: (string -> unit) ref =
  ref (fun _ -> failwith "uninitialized add_iter")

let uninitialized_install_bindings =
  fun _ -> failwith "uninitialized install_bindings"

let init_rule: (unit -> unit) ref =
  ref uninitialized_install_bindings

let install_bindings: (string -> unit) ref =
  ref uninitialized_install_bindings

(* ---------------------------------------------------------------------- *)
(* String format things *)

let format_metavariables      =
  ref ([] : (string * (Ast.meta_name * iconstraints)) list)
let format_list_metavariables =
  ref ([] : (string * (Ast.meta_name * Ast.list_len)) list)
