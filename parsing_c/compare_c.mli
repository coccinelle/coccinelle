type compare_result = 
  | Correct 
  | Incorrect of string
  | IncorrectOnlyInNotParsedCorrectly


val compare : 
 Ast_c.program2 * Parse_c.parsing_stat -> 
 Ast_c.program2 * Parse_c.parsing_stat ->
 compare_result * string list (* the output of diff *)
