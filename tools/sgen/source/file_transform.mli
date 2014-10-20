(* Transforms the original Coccinelle script and prints it (transformation is
 * done while printing).
 *
 * Prints preface, added virtual rules.
 * In patch mode: add rule names to unnamed rules, add patch dependencies.
 * In context mode: skip the rules.
 *
 * Rules must be sorted in order of when they occur in the script.
 *)

(* ------------------------------------------------------------------------- *)
(* TRANSFORMATION FUNCTIONS *)

val print :
  channel:out_channel ->
  file_name:string ->
  preface:string ->
  virtuals:string list ->
  rules:(Ast0_cocci.parsed_rule * string option (* new name *)) list ->
  context_mode:bool ->
  unit
