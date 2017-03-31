(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Handles disjunctions.

 * Disjunctions need special rule generation hence the separate module:
 *  - The positions have to be the same over each disjunction case. Therefore
 *    there can at most be generated one position per disjunction case. Also,
 *    it has to be the exact same position for all cases.
 *
 *  - Disjunctions can be used for pattern matching in Coccinelle rules, where
 *    only some cases are matched/patched. However, this may create false
 *    positives when generating a context rule from a patch rule. Therefore two
 *    rules have to be generated (aka the ugly part).
 *    This is the case when there is:
 *     - something interesting outside the disjunction
 *     - something uninteresting inside the disjunction
 *     - something interesting inside the disjunction
 *
 *  - Disregard the above rule in cases where there is no risk of false
 *    positives - e.g. if the disjunction is the only thing in the rule.
 *    Determined by at_top flag.
 *
 * ----------------------------------------------------------------------------
 * Example of rule splitting:
 *
 *      @disj@ expression n; identifier x; @@
 *
 *      - x = call(x);
 *      (
 *       some_function(0);
 *      |
 *      - some_function(n);
 *      + some_function(x);
 *      )
 *
 * The following would NOT work:
 *
 *      @disj@ expression n; identifier x; position p1,p2; @@
 *
 *      * x@p1 = call(x);
 *      (
 *       some_function(0);
 *      |
 *      * some_function@p2(n);
 *      )
 *
 * because that would match even when some_function is called on 0 (due to the
 * first star) which we don't want. Instead we want two rules:
 *
 *      @disj1@ expression n; identifier x; position p1,p2; @@
 *
 *      x@p1 = call(x);
 *      (
 *       some_function(0);
 *      |
 *       some_function@p2(n);
 *      )
 *
 *      @disj2@ expression n; identifier x; position disj1.p1, disj1.p2 @@
 *
 *      * x@p1 = call(x);
 *      * some_function@p2(n);
 *
 * which would match if and only if n is not 0 (in this particular example).
 *)

(* ------------------------------------------------------------------------- *)
(* DISJ GENERATION FUNCTIONS *)

(* These types are just aliases for functions that take some AST0 component and
 * a snapshot, handle the component with regards to the input snapshot and
 * return the resulting snapshot.
 *)

type statement_dots_fn =
  Ast0_cocci.statement Ast0_cocci.dots -> Snapshot.t ->  Snapshot.t

type string_fn = string Ast0_cocci.mcode -> Snapshot.t -> Snapshot.t

type statement_fn = Ast0_cocci.statement -> Snapshot.t -> Snapshot.t

type expression_fn = Ast0_cocci.expression -> Snapshot.t -> Snapshot.t

type ident_fn = Ast0_cocci.ident -> Snapshot.t -> Snapshot.t

type declaration_fn = Ast0_cocci.declaration -> Snapshot.t -> Snapshot.t

type field_fn = Ast0_cocci.field -> Snapshot.t -> Snapshot.t

(* These functions take a disjunction component (stmt, expr, ident, decl) and a
 * snapshot + some other things, and perform the disjunction rule generation.
 * Fails if the disjunction component is NOT a disjunction.
 *
 * All functions take:
 *  - strfn: a function to handle string_mcodes.
 *  - some Ast0 component, ie. the thing that contains the disjunction.
 *  - some function to handle the Ast0 component.
 *  - at_top: a flag that indicates whether it is safe to just generate one
 *     rule, even though the disjunction uses pattern matching.
 *
 * Return:
 *  - an updated snapshot with the inserted disjunction, possibly an extra
 *    disjunction rule if needed.
 *)

val generate_statement :
  stmtdotsfn:statement_dots_fn ->
  strfn:string_fn ->
  stmtfn:statement_fn ->
  stmt:Ast0_cocci.statement ->
  at_top:bool ->
  Snapshot.t ->
  Snapshot.t

val generate_expression :
  strfn:string_fn ->
  exprfn:expression_fn ->
  expr:Ast0_cocci.expression ->
  at_top:bool ->
  Snapshot.t ->
  Snapshot.t

val generate_ident :
  strfn:string_fn ->
  identfn:ident_fn ->
  ident:Ast0_cocci.ident ->
  at_top:bool ->
  Snapshot.t ->
  Snapshot.t

val generate_declaration :
  strfn:string_fn ->
  declfn:declaration_fn ->
  decl:Ast0_cocci.declaration ->
  at_top:bool ->
  Snapshot.t ->
  Snapshot.t

val generate_field :
  strfn:string_fn ->
  fieldfn:field_fn ->
  field:Ast0_cocci.field ->
  at_top:bool ->
  Snapshot.t ->
  Snapshot.t
