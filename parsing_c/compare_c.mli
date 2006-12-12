open Common

type compare_result = 
  | Correct 
  | Incorrect of string
  | IncorrectOnlyInNotParsedCorrectly


val compare : 
 (Ast_c.program * filename) -> (Ast_c.program * filename) ->
 compare_result * string list (* the output of diff *)
