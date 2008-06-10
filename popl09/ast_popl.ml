type sequence =
    Seq of element * sequence
  | Empty
  | SExists of Ast_cocci.meta_name * sequence

and term =
    Term of Ast_cocci.rule_elem
  | TExists of Ast_cocci.meta_name * term

and element =
    Atomic of term * dots_bef_aft
  | IfThen of term * element * Ast_cocci.end_info * dots_bef_aft
  | Or of sequence * sequence
  | DInfo of dots
  | EExists of Ast_cocci.meta_name * element

and dots = 
    Dots
  | Nest of sequence
  | When of dots * sequence

and dots_bef_aft =
    NoDots
  | AddingBetweenDots of element * int (*index of let var*)
  | DroppingBetweenDots of element * int (*index of let var*)

