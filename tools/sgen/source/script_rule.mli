(*
 * Creates two script rules like:
 *
 *      @script:python depends on org@
 *      p1 << rulename.p1;
 *      p2 << rulename.p2;
 *      x << rulename.x;
 *      @@
 *
 *      msg="Warning: This is a message! Found %s." % (x)
 *      coccilib.org.print_safe_todo(p1[0], msg)
 *      coccilib.org.print_link(p2[0], "")
 *
 *
 *      @script:python depends on report@
 *      p1 << rulename.p1;
 *      p2 << rulename.p2;
 *      @@
 *
 *      msg="Error: This is a report message on line %s" % (p2[0].line)
 *      coccilib.report.print_report(p1[0], msg)
 *)

(* ------------------------------------------------------------------------- *)
(* SCRIPT GENERATION FUNCTIONS *)

type t

(* generates org and report script for one rule.
 * metapos is the list of added metapositions that show where the match is.
 * user_rule contains the rule name, the rule messages and the metavars.
 *
 * INVARIANT: there MUST be at least one position in the metapos list!!!
 *)
val generate :
  meta_pos:Meta_variable.t list ->
  user_rule:User_input.Rule.t ->
  t

(* prints org rule *)
val print_org : out_channel -> t -> unit

(* prints report rule *)
val print_report : out_channel -> t -> unit
