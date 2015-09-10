(* Get user input from interactive (commandline) mode.
 * Returns the user input, formatted for use, and gives the user the
 * opportunity to save the input in a config file.
 *)

(* ------------------------------------------------------------------------- *)
(* INTERACTION FUNCTIONS *)

(* Launches interactive mode.
 *
 * Arguments:
 *  - rule_names are the names of all */+/- rules
 *  - config_name is the suggested name for persisting the user input.
 *
 * Returns:
 *  - preface (description, comments, etc.) formatted for use in a string
 *  - a list of user inputs, one per rule
 *)
val interact :
  rule_names:string list ->
  config_name:string ->
  User_input.t
