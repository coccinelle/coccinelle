// This file is part of Coccinelle, lincensed under the terms of the GPL v2.
// See copyright.txt in the Coccinelle source code for more information.
// The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

/// Description for the disj script.
//# This is a limitation.
//# This is another limitation.
///
// Confidence: Moderate
// Comments: This is to test disjunction generation.

virtual patch
virtual context
virtual org
virtual report

@disj depends on patch && !context && !org && !report@
expression E1, E2, E3;
identifier x;
@@

if (E2 <
- E3
+ E1
 ) { ...
(
some_function(0);
x = 30;
|
some_function(1);
x = 30;
|
- this(E1);
+ that(E2);
|
- some_function(E1)
+ another_function(E2)
;
- x = 30;
)
... }

// ----------------------------------------------------------------------------

@disj_context depends on !patch && (context || org || report) exists@
identifier x;
expression E1, E2, E3;
position j0, j1;
@@

if (E2@j0 <
 E3
 ) { ...
(
some_function(0);
x = 30;
|
some_function(1);
x = 30;
|
 this@j1(E1);
|
 some_function@j1(E1)
;
 x = 30;
)
... }

@disj_disj depends on !patch && (context || org || report)@
identifier x;
expression E1, E2, E3;
position disj_context.j0, disj_context.j1;
@@

if (E2@j0 <
*  E3
 ) { ...
(
*  this@j1(E1);
|
*  some_function@j1(E1)
;
*  x = 30;
)
... }

// ----------------------------------------------------------------------------

@script:python disj_org depends on org@
j0 << disj_context.j0;
j1 << disj_context.j1;
@@

msg = "Org message. Found a match in disj rule.."
coccilib.org.print_todo(j0[0], msg)
coccilib.org.print_link(j1[0], "")

// ----------------------------------------------------------------------------

@script:python disj_report depends on report@
E1 << disj_context.E1;
E2 << disj_context.E2;
j0 << disj_context.j0;
j1 << disj_context.j1;
@@

msg = "Report message. Remove call to this(%s) and replace with call to that(%s) around line %s." % (E1,E2,j1[0].line)
coccilib.report.print_report(j0[0], msg)

