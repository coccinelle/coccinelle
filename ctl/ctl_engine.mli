
val sat :
  ('a, 'b, 'c, 'd, 'e, 'f) Ast_ctl.model ->
  ('a, 'b) Ast_ctl.generic_ctl ->
  (Ograph_extended.nodei * ('b, 'c) Ast_ctl.generic_substitution *
   (Ograph_extended.nodei, ('b, 'c) Ast_ctl.generic_substitution, 'd list)
   Ast_ctl.generic_witness list)
  list
