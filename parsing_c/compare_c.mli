open Common

type compare_result = 
  | Correct 
  | Pb of string
  | PbOnlyInNotParsedCorrectly


val compare : 
 (Ast_c.program * filename) -> (Ast_c.program * filename) ->
 compare_result * string list (* the output of diff *)
