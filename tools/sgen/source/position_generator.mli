(* Generates a position in an appropriate place, adds it to the snapshot.
 * Returns None if no position could be generated (this is the case in e.g.
 * ... (dots) because they don't really have a well-defined position).
 *)

(* ------------------------------------------------------------------------- *)
(* POSITION GENERATION FUNCTIONS *)

(* transforms the given statement into one with an added metaposition *)
val statement_pos :
  Ast0_cocci.statement ->
  Generator_types.snapshot ->
  (Ast0_cocci.statement * Generator_types.snapshot) option

(* transforms the given expression into one with an added metaposition *)
val expression_pos :
  Ast0_cocci.expression ->
  Generator_types.snapshot ->
  (Ast0_cocci.expression * Generator_types.snapshot) option

(* transforms the given declaration into one with an added metaposition *)
val declaration_pos :
  Ast0_cocci.declaration ->
  Generator_types.snapshot ->
  (Ast0_cocci.declaration * Generator_types.snapshot) option

(* transforms the given identifier into one with an added metaposition *)
val ident_pos :
  Ast0_cocci.ident ->
  Generator_types.snapshot ->
  Ast0_cocci.ident * Generator_types.snapshot
