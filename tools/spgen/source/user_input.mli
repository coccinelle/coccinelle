(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Types and functionality for user input
 * Turns the raw input from the config/interactive into printable strings and
 * information ready to use by the generators.
 *)

(* ------------------------------------------------------------------------- *)
(* CONFIDENCE *)

(* The confidence level of the script. Low, Moderate, or High. *)

module Confidence : sig
  type t = Low | Moderate | High

  exception Not_confidence of string

  val to_string : t -> string

  (* fails with Not_confidence if the string is not l, m, h, low, moderate, or
   * high. Case insensitive.
   *)
  val from_string : string -> t
end


(* ------------------------------------------------------------------------- *)
(* USER RULE *)

(* user-specified data (new name + org and report messages) for one rule. *)

module Rule : sig
  type t

  (* rule_name is the new rulename (or the original one if a new one is not
   * needed). org and report are (error message, list of metavariables).
   *)
  val make :
    rule_name:string ->
    org:(string * string list) ->
    report:(string * string list) ->
    t

  val get_name : t -> string
  val get_org : t -> string * Meta_variable.t list
  val get_report : t -> string * Meta_variable.t list
end


(* ------------------------------------------------------------------------- *)
(* USER INPUT *)

(* The user input type. encapsulates the stuff that is specified by the user*)
type t

(* constructor; description and confidence level are required *)
val make : description:string -> confidence:Confidence.t -> t

(* setters *)
val set_keys : string -> t -> t
val set_conf : Confidence.t -> t -> t
val set_comments : string -> t -> t
val set_options : string -> t -> t
val set_url : string -> t -> t
val set_limits : string list -> t -> t
val add_limit : string -> t -> t
val set_authors : string list -> t -> t
val add_author : string -> t -> t
val add_rule : rule_name:string -> Rule.t -> t -> t

(* check if there's already a rule with that name *)
val check_name : string -> t -> unit

(* get string formatted version of preface *)
val get_preface : t -> string

(* returns rule with associated metadata, default generated if not found *)
val get_rule : rule_name:string -> t -> Rule.t

(* turns a user input into the config that generates it *)
val unparse : t -> string

(* format string checking *)
val check_format_string : string * string list -> unit
val count_format_vars : string -> int
