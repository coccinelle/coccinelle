(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

(* Get user input from config file.
 *
 *  - The configs are centered around the syntax:
 *    <attribute> = <value>
 *    using newlines as delimiters.
 *  - For attributes with multiple values, the values are delimitered by pipes |
 *  - Rules follow the syntax:
 *    <rulename> =
 *      org: "here is a message. found %s!" % e
 *      report: "this is an org message."
 *    where the messages follow the Python format string syntax. If only one of
 *    org or report is specified, the same message is used for both.
 *  - <rulename> can be the literal rule name, or, if none:
 *    <line number that rule is declared>:<new rulename>
 *  - Each attribute has a one-letter shorthand, usually the first letter.
 *  - Comments follow C syntax, ie. // for one-line and /**/ for multi-line.
 *
 * See the lexer file for specifics.
 *)

(* ------------------------------------------------------------------------- *)
(* PARSER FUNCTIONS *)

(* read local config with information specific for the coccinelle script
 * currently being generated.
 * rule_names are a list of the names of */+/- rules in the coccinelle script.
 *)
val parse_local :
  rule_names:string list ->
  config_name:string ->
  User_input.t

(* no user input, default is:
 * description = No description.
 * confidence = Moderate
 *)
val parse_default : User_input.t

(* read global config for general information in any rule generation.
 * TODO: implement.
 *)
val parse_global :
  config_name:string ->
  string option * (* Author name *)
  string option * (* License *)
  string * (* Default generated rule name *)
  string * (* Default generated position name *)
  string * (* Default error message for org and report *)
  int (* Character limit (width) for generated script *)
