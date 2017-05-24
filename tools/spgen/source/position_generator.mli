(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Given some Ast0 component x, generates a position at an appropriate place
 * and returns
 *  - Some (x,snp), where x has an inserted metaposition and snp is the updated
      snapshot.
 *  - None, if no position could be generated (this is the case in e.g. dots
 *    (...) because they don't really have a well-defined position).
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

val ident_pos :
  Ast0_cocci.ident ->
  Snapshot.t ->
  Ast0_cocci.ident * Snapshot.t

val expression_pos :
  Ast0_cocci.expression ->
  Snapshot.t ->
  (Ast0_cocci.expression * Snapshot.t) option

val declaration_pos :
  Ast0_cocci.declaration ->
  Snapshot.t ->
  (Ast0_cocci.declaration * Snapshot.t) option

val field_pos :
  Ast0_cocci.field ->
  Snapshot.t ->
  (Ast0_cocci.field * Snapshot.t) option

val statement_pos :
  Ast0_cocci.statement ->
  Snapshot.t ->
  (Ast0_cocci.statement * Snapshot.t) option
