@r@
identifier i;
constant C;
@@

i = C;

@script:python@
i << r.i;
@@

print i
