@s@
expression E;
@@

g(E);

@r@
expression s.E, E1;
@@

f(E,E1);

@script:python@
E1 << r.E1;
@@

print E1
