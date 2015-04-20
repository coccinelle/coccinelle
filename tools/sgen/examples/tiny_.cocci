/// This is a description for the Coccinelle script tiny.cocci.
//# limitation 1.
//# limitation 2.
///
// Confidence: Low
// Copyright: (C) 2015 Karl Koder, DIKU. Some license.
// Copyright: (C) 2015 Hanne Hacker, DIKU. Some license.
// URL: http://coccinelle.lip6.fr/
// Comments: Further comments.
// Options: --option-1, --option-2, --option-3.
// Keywords: tiny, negative, assignment.

virtual patch
virtual context
virtual org
virtual report

@some_rule depends on patch && !context && !org && !report@
int e;
identifier i;
@@

  int i = e;
+ if (i < 0) i = 0;
  ...

// ----------------------------------------------------------------------------

@some_rule_context depends on !patch && (context || org || report)@
identifier i;
int e;
position j0;
@@

*   int i@j0 = e;
  ...

// ----------------------------------------------------------------------------

@script:python some_rule_org depends on org@
j0 << some_rule_context.j0;
@@

msg = "WARNING: Check for negative value in assignment."
coccilib.org.print_todo(j0[0], msg)

// ----------------------------------------------------------------------------

@script:python some_rule_report depends on report@
j0 << some_rule_context.j0;
@@

msg = "Check for negative value in assignment."
coccilib.report.print_report(j0[0], msg)

