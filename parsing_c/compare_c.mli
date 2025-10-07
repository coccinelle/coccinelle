type compare_result =
  | Correct
  | Pb of string
  | PbOnlyInNotParsedCorrectly of string
  | PbKnown of string


(* the string list is the output of diff *)

val compare_default : (* compare to a res file *)
  bool -> Common.filename -> Common.filename -> compare_result * string list

val compare_to_original : (* compare to the source of the transformation *)
  Common.filename -> Common.filename -> compare_result * string list

val exact_compare : (* compare to a res file using diff (check spacing) *)
  bool -> Common.filename -> Common.filename -> compare_result * string list


val compare_result_to_string : compare_result * string list -> string
val compare_result_to_bool : compare_result -> bool
