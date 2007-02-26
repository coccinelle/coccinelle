open Common

type compare_result = 
  | Correct 
  | Pb of string
  | PbOnlyInNotParsedCorrectly of string


(* the string list is the output of diff *)

val compare_ast : 
 filename -> filename -> compare_result * string list 

val compare_token :
  filename -> filename -> compare_result * string list
