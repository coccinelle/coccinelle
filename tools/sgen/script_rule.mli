(*
 * Creates two script rules like:
 *
 * @script:python depends on org@
 * p1 << rulename.p1;
 * p2 << rulename.p2;
 * x << rulename.x;
 * @@
 * 
 * msg="Warning: This is a message! Found %s." % (x)
 * coccilib.org.print_safe_todo(p1[0], msg)
 * coccilib.org.print_link(p2[0], "")
 * 
 * 
 * @script:python depends on report@
 * p1 << rulename.p1;
 * p2 << rulename.p2;
 * @@
 * 
 * msg="Error: This is a report message on line %s" % (p2[0].line)
 * coccilib.report.print_report(p1[0], msg)
 *)

(* ------------------------------------------------------------------------- *)
(* SCRIPT GENERATION FUNCTIONS *)

type t

(* generates org and report script for one rule.
 * metapos is the list of added metapositions that show where the match is.
 * INVARIANT: there MUST be at least one position in the metapos list!!! *)
val generate :
  metapos:Meta_variable.t list ->
  user_input:
    (string * string option) *         (* old rulename, new rulename *)
    (string * Meta_variable.t list) *  (* org message and metavars *)
    (string * Meta_variable.t list) -> (* report message and metavars *)
  t

(* prints the script rules, org first, then report *)
val print : out_channel -> t -> unit

(* print a list of script rules, grouping the org and the report rules
 * the (unit -> unit) function is called between org and report *)
val print_split : out_channel -> t list -> (unit -> unit) -> unit
