(* Get user input from interactive (commandline) mode.
 * Returns the user input, formatted for use, and gives the user the
 * opportunity to save the input in a config file.
 *)

(* ------------------------------------------------------------------------- *)
(* INTERACTION FUNCTIONS *)

(* Launches interactive mode.
 * ordered_rules are the names of all */+/- rules, ordered by occurrence in
 * the script.
 * config_name is the suggested name for persisting the user input.
 *)
val interact :
  ordered_rules:string list ->
  config_name:string ->
  string *                           (* preface *)
  ((string * string option) *        (* old rulename, new rulename *)
   (string * Meta_variable.t list) * (* org *)
   (string * Meta_variable.t list)   (* rep *)
  ) list
