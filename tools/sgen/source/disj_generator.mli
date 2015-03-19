(* Handles disjunctions.

 * Disjunctions need special rule generation hence the separate module:
 *   - The positions have to be the same over each disjunction case. Therefore
 *     there can at most be generated one position per disjunction case. Also,
 *     it has to be the exact same position for all cases.
 *
 *   - Disjunctions can be used for pattern matching in Coccinelle rules, where
 *     only some cases are matched/patched. However, this may create false
 *     positives when generating a context rule from a patch rule. Therefore
 *     two rules have to be generated (aka the ugly part).
 *
 *   - Disregard the above rule in cases where there is no risk of false
 *     positives - e.g. if the disjunction is the only thing in the rule.
 *     Determined by at_top flag.
 *
 * All functions take:
 *   - strfn: a function to handle string_mcodes.
 *   - some Ast0 component, ie. the thing that contains the disjunction.
 *   - some function to handle the Ast0 component.
 *   - at_top: a flag that indicates whether it is safe to just generate one
 *     rule, even though the disjunction uses pattern matching.
 *     TODO: better detection of when to set the at_top flag (in rule_body).
 *
 * Returns:
 *   - an updated snapshot with the inserted disjunction, possible an extra
 *     disjunction rule if needed.
 *)

(* ------------------------------------------------------------------------- *)
(* DISJ GENERATION FUNCTIONS *)

type statement_dots_fn =
  (Ast0_cocci.statement Ast0_cocci.dots -> Generator_types.snapshot ->
   Generator_types.snapshot)

type string_fn =
  (string Ast0_cocci.mcode -> Generator_types.snapshot ->
   Generator_types.snapshot)

type statement_fn =
  (Ast0_cocci.statement -> Generator_types.snapshot ->
   Generator_types.snapshot)

type expression_fn =
  (Ast0_cocci.expression -> Generator_types.snapshot ->
   Generator_types.snapshot)

type ident_fn =
  (Ast0_cocci.ident -> Generator_types.snapshot ->
   Generator_types.snapshot)

type declaration_fn =
  (Ast0_cocci.declaration -> Generator_types.snapshot ->
   Generator_types.snapshot)

val generate_statement :
  stmtdotsfn:statement_dots_fn ->
  strfn:string_fn ->
  stmtfn:statement_fn ->
  stmt:Ast0_cocci.statement ->
  at_top:bool ->
  Generator_types.snapshot ->
  Generator_types.snapshot

val generate_expression :
  strfn:string_fn ->
  exprfn:expression_fn ->
  expr:Ast0_cocci.expression ->
  at_top:bool ->
  Generator_types.snapshot ->
  Generator_types.snapshot

val generate_ident :
  strfn:string_fn ->
  identfn:ident_fn ->
  ident:Ast0_cocci.ident ->
  at_top:bool ->
  Generator_types.snapshot ->
  Generator_types.snapshot

val generate_declaration :
  strfn:string_fn ->
  declfn:declaration_fn ->
  decl:Ast0_cocci.declaration ->
  at_top:bool ->
  Generator_types.snapshot ->
  Generator_types.snapshot
