(* Given some Ast0 component x, generates a position at an appropriate place
 * and returns
 *  - Some x, where x has an inserted position
 *  - None, if no position could be generated (this is the case in e.g. dots
 *    (...) because they don't really have a well-defined position.
 *
 * "appropriate place" means usually after an id if possible.
 * The added position is always in an Ast0.PLUS context (using the fact
 * that a metaposition in the original script is NEVER in plus context).
 *
 * Note that it might return Some <component> even if no position was inserted
 * if the component is optional (ie. on a line preceded with ? in SmPL)
 *)

(* ------------------------------------------------------------------------- *)
(* POSITION GENERATION FUNCTIONS *)

(* transforms the given statement into one with an added metaposition *)
val statement_pos :
  Ast0_cocci.statement ->
  Snapshot.t ->
  (Ast0_cocci.statement * Snapshot.t) option

(* transforms the given expression into one with an added metaposition *)
val expression_pos :
  Ast0_cocci.expression ->
  Snapshot.t ->
  (Ast0_cocci.expression * Snapshot.t) option

(* transforms the given declaration into one with an added metaposition *)
val declaration_pos :
  Ast0_cocci.declaration ->
  Snapshot.t ->
  (Ast0_cocci.declaration * Snapshot.t) option

(* transforms the given identifier into one with an added metaposition *)
val ident_pos :
  Ast0_cocci.ident ->
  Snapshot.t ->
  Ast0_cocci.ident * Snapshot.t
