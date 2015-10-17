(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Transforms the original Coccinelle script and prints it (transformation is
 * done while printing).
 *
 * Prints preface, added virtual rules.
 *
 * Patch mode:
 *  - add rule names to previously unnamed rules.
 *  - add standard dependencies to existing patch rules.
 *
 * Context mode:
 *  - skip all original context rules, since we now have the same rules but in
 *    a generated (and therefore superior!) version.
 *
 * The transformation is done alongside the printing so if anything fails,
 * some of it might already have been printed.
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

(* rules must be in the same order that they occurred in the original file *)
val print :
  context_mode:bool ->
  file_name:string ->
  preface:string ->
  virtuals:string list ->
  ordered_rules:(Ast0_cocci.parsed_rule * string (* new name *)) list ->
  out_channel ->
  unit
