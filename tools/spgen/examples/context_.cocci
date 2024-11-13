// This file is part of Coccinelle, lincensed under the terms of the GPL v2.
// See copyright.txt in the Coccinelle source code for more information.
// The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/

/// A generated context rule! This is a Coccinelle script that tests the use of
/// the local function metavariable type in SmPL. This comment is longer than
/// 80 characters, so it will be split.
///
// Confidence: Low

virtual context
virtual org
virtual report

// ----------------------------------------------------------------------------

@localf depends on context || org || report@
identifier m, x, y;
local function f;
position j0, j1, j2;
@@

  f@j0(
*    int x,
    int y
  ) {
  int m@j1 = x * y;
  ...
  return m@j2;
}

// ----------------------------------------------------------------------------

@script:python localf_org depends on org@
x << localf.x;
y << localf.y;
j0 << localf.j0;
j1 << localf.j1;
j2 << localf.j2;
@@

msg = "Found a function with arguments %s and %s that multiplies its arguments " % (x,y)
coccilib.org.print_safe_todo(j0[0], msg)
coccilib.org.print_link(j1[0], "")
coccilib.org.print_link(j2[0], "")

// ----------------------------------------------------------------------------

@script:python localf_report depends on report@
x << localf.x;
y << localf.y;
j0 << localf.j0;
j1 << localf.j1;
j2 << localf.j2;
@@

msg = "ERROR: Found a function with arguments %s and %s that are multiplied around lines %s,%s." % (x,y,j1[0].line,j2[0].line)
coccilib.report.print_report(j0[0], msg)

