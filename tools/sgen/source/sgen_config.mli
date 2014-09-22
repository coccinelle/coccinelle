(* Get user input from config file.
 *
 * * The configs are centered around the syntax:
 *   <attribute> = <value>
 *   using newlines as delimiters.
 * * For attributes with multiple values, the values are delimitered by pipes |
 * * Rules follow the syntax:
 *   <rulename> =
 *     org: "here is a message. found %s!" % e
 *     report: "this is an org message."
 *   where the messages follow the Python format string syntax. If only one of
 *   org or report is specified, the same message is used for both.
 * * <rulename> can be the literal rule name, or, if none:
 *   <line number that rule is declared>:<new rulename>
 * * Each attribute has a one-letter shorthand, usually the first letter.
 * * Comments follow C syntax, ie. // for one-line and /**/ for multi-line.
 *
 * See the lexer file for specifics.
 *)

(* ------------------------------------------------------------------------- *)
(* PARSER FUNCTIONS *)

(* read local config with information specific for the coccinelle script
 * currently being generated.
 *)
val parse_local :
  ordered_rules:string list ->
  config_name:string ->
  string *                           (* preface *)
  ((string * string option) *        (* old rulename, new rulename *)
   (string * Meta_variable.t list) * (* org *)
   (string * Meta_variable.t list)   (* rep *)
  ) list

(* no user input, default is:
 * description = No description.
 * confidence = Moderate *)
val parse_default :
  ordered_rules:string list ->
  string *                           (* preface *)
  ((string * string option) *        (* old rulename, new rulename *)
   (string * Meta_variable.t list) * (* org *)
   (string * Meta_variable.t list)   (* rep *)
  ) list

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
