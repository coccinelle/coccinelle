@r@
format d =~ ".x$";
@@

foo("...%@d@...")

@script:python@
d << r.d;
@@

d = "%s" % (d)
assert(d == "2x")

@r1@
format list d;
@@

foo("blah %@d@ xxx %d")

@script:python@
d << r1.d;
@@

d = "%s" % (d)
assert(d == "%d two %2x three %s")

@r2@
format list d;
@@

foo("%@d@ xxx %d")

@script:python@
d << r2.d;
@@

d = "%s" % (d)
assert(d == "blah %d two %2x three %s")

@r3@
format list[4] d;
@@

"xyz %@d@"

@script:python@
d << r3.d;
@@

d = "%s" % (d)
assert(d == "%d %d %0.2f %s three\\n")

@r4@
format list[2] d;
@@

"xyz %@d@ abc"

@script:python@
d << r4.d;
@@

d = "%s" % (d)
assert(d == "%d %0.2f")

@r5@
format d;
@@

- "xxx %@d@"
+ "yyy %@d@"
