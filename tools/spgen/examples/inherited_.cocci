// This file is part of Coccinelle, lincensed under the terms of the GPL v2.
// See copyright.txt in the Coccinelle source code for more information.
// The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

/// This is a Coccinelle script to test inheritance between rules.
//# Only works for functions with exact names f, g, h, hh.
//# Not really useful.
///
// Confidence: High
// Copyright: (C) 2015 Author1, affiliation. License1
// Copyright: (C) 2015 Author2, affiliation. License2
// Copyright: (C) 2015 Author3.
// URL: http://coccinelle.lip6.fr/
// Comments: Additional comments.
// Options: --a-flag, --another-flag, --a-third-flag.
// Keywords: inheritance, test.

virtual patch
virtual context
virtual org
virtual report

@r@
expression x;
@@

(
 f(x);
|
 g(1);
)

@rule_h depends on patch && !context && !org && !report@
expression r.x;
@@

- h(x);
+ hh(x);


@rule_h2 depends on patch && !context && !org && !report@
expression r.x;
@@
- h2(x);
+ hh22(x);

@rule_0 depends on patch && !context && !org && !report@
@@

- foo(1);
+ bar(1);

// ----------------------------------------------------------------------------

@rule_h_context depends on !patch && (context || org || report)@
expression r.x;
position j0;
@@

*  h@j0(x);

@rule_h2_context depends on !patch && (context || org || report)@
expression r.x;
position j0;
@@

*  h2@j0(x);

@rule_0_context depends on !patch && (context || org || report)@
position j0;
@@

*  foo@j0(1);

// ----------------------------------------------------------------------------

@script:python rule_h_org depends on org@
x << r.x;
j0 << rule_h_context.j0;
@@

msg = "Replace call to h with call to hh on expression %s. " % (x)
coccilib.org.print_safe_todo(j0[0], msg)

@script:python rule_h2_org depends on org@
j0 << rule_h2_context.j0;
@@

msg = "This is a message for rule_h2 in org mode.."
coccilib.org.print_todo(j0[0], msg)

@script:python rule_0_org depends on org@
j0 << rule_0_context.j0;
@@

msg = "found a match around here ...."
coccilib.org.print_todo(j0[0], msg)

// ----------------------------------------------------------------------------

@script:python rule_h_report depends on report@
x << r.x;
j0 << rule_h_context.j0;
@@

msg = "Replace call to h with call to hh on expression %s. " % (x)
coccilib.report.print_report(j0[0], msg)

@script:python rule_h2_report depends on report@
j0 << rule_h2_context.j0;
@@

msg = "This is a message for rule_h2 in report mode.."
coccilib.report.print_report(j0[0], msg)

@script:python rule_0_report depends on report@
j0 << rule_0_context.j0;
@@

msg = "found a match around here ...."
coccilib.report.print_report(j0[0], msg)

