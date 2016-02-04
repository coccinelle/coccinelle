(* Transforms the original Coccinelle script and prints it (transformation is
 * done while printing).
 *
 * Prints preface, added virtual rules.
 * In patch mode: add rule names to unnamed rules, add patch dependencies.
 * In context mode: skip the rules.
 *
 * Rules must be sorted in order of when they occur in the script.
 *
 * ----------------------------------------------------------------------------
 * Example:
 * Input file (ie. the one in file_name) is:
 *
 *      @im_a_rule@
 *      @@
 *
 *      function();
 *
 *      @depends on im_a_rule@
 *      @@
 *
 *      another_function( ... );
 *      + call_me();
 *      ...
 *
 * Then if we call it with e.g.
 * preface = { confidence = Moderate; description = "this is a script" },
 * virtuals = ["patch";"context";"org";"report"],
 * rules = [(<im_a_rule>, None); (<rule starting on line 6>, Some "NAME")],
 * context_mode = false (because it has a +),
 * it would print the following to the out_channel:
 *
 *      /// this is a script
 *      ///
 *      // Confidence: Moderate
 *
 *      virtual patch
 *      virtual context
 *      virtual org
 *      virtual report
 *
 *      @im_a_rule@
 *      @@
 *
 *      function();
 *
 *      @NAME depends on im_a_rule && patch && !context && !org && !report@
 *      @@
 *
 *      another_function( ... );
 *      + call_me();
 *      ...
 *)

(* ------------------------------------------------------------------------- *)
(* TRANSFORMATION FUNCTIONS *)

val print :
  context_mode:bool ->
  file_name:string ->
  preface:string ->
  virtuals:string list ->
  ordered_rules:(Ast0_cocci.parsed_rule * string (* new name *)) list ->
  out_channel ->
  unit
