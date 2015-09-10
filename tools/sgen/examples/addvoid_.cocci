/// Adds void to function headers with no arguments.
//# If a matched function has a prototype declaration, the script will not
//# match the prototype.
///
// Confidence: High
// URL: http://coccinelle.lip6.fr
// Options: --recursive-includes

virtual patch
virtual context
virtual org
virtual report

@addvoid depends on patch && !context && !org && !report@
identifier f;
@@

f(
+ void
 ) { ... }

// ----------------------------------------------------------------------------

@addvoid_context depends on !patch && (context || org || report)@
identifier f;
position j0;
@@

* f@j0(
 ) { ... }

// ----------------------------------------------------------------------------

@script:python addvoid_org depends on org@
f << addvoid_context.f;
j0 << addvoid_context.j0;
@@

msg = "WARNING: Zero-argument function \"%s\" should have void declaration. " % (f)
coccilib.org.print_safe_todo(j0[0], msg)

// ----------------------------------------------------------------------------

@script:python addvoid_report depends on report@
f << addvoid_context.f;
j0 << addvoid_context.j0;
@@

msg = "WARNING: Zero-argument function \"%s\" should have void declaration. " % (f)
coccilib.report.print_report(j0[0], msg)

