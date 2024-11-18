(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

(* Global variables, common string functions, and helpers. *)

(* ------------------------------------------------------------------------- *)
(* GLOBAL VARIABLES *)

(* global variables are readonly outside initialisation *)
val init :
  rule_name:string -> (* default rule name for nameless rules *)
  pos_name:string ->  (* default position name for generated positions *)
  error_msg:string -> (* default error message for org and report mode *)
  char_limit:int ->   (* page width limit for generated script *)
  unit

(* default position name *)
val get_pos_name : unit -> string

(* default error message *)
val get_default_message : unit -> string

(* use input rulename to make a name for its context counterpart. *)
val get_context_name : context_mode:bool -> string -> string

(* use input rulename to make a name for its disjunction counterpart. *)
val get_disj_name : string -> string

(* use input rulename to make a name for its org counterpart. *)
val get_org_name : string -> string

(* use input rulename to make a name for its report counterpart. *)
val get_report_name : string -> string


(* ------------------------------------------------------------------------- *)
(* DEPENDENCY HELPERS (HARDCODED) *)

(* takes existing virtual rulenames, checks them, and returns the standard
 * ones: (patch), context, org, and report.
 *)
val key_virtuals : context_mode:bool -> string list -> string list

(* takes rule dependencies and adds default patch dependency:
 * "<existing dependency> && patch && !context && !org && !report"
 *)
val add_patch_dependency :
  Ast_cocci.dependency -> Ast_cocci.dependency

(* takes rule dependencies and adds default context dependency:
 * if context_mode: "<existing dependency && (context || org || report)"
 * else patch: "<existing dependency> && !patch && (context || org || report)"
 *)
val add_context_dependency :
  context_mode:bool -> Ast_cocci.dependency -> Ast_cocci.dependency


(* ------------------------------------------------------------------------- *)
(* SANITY CHECKS AND RULE HELPERS *)

(* specific for nameless rule that are automatically given the name
 * "rule starting on line <number>". extracts the number or fails if none.
 *)
val extract_line : string -> int

(* check rulename for validity. fails if invalid.
 * if strict, also fail on rulenames containing spaces.
 *)
val check_rule : strict:bool -> string -> unit

(* takes a rulename; if it is invalid, generates and returns a new rulename.
 * if valid, returns the input name.
 *)
val generate_rule : string -> string


(* ------------------------------------------------------------------------- *)
(* STRING FUNCTIONS *)

val starts_with_digit : string -> bool

(* splits string into string list of length at most char_limit, delimitering
 * by space, and prefixing each string with prefix. *)
val pre_split : ?prefix:string -> string -> string

(* same as above, except for string options. If None, return "". *)
val pre_split_opt : ?prefix:string -> string option -> string

val concat_limit_width : string list -> string
