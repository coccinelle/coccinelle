@r@
identifier f;
expression list es;
position p;
@@

f(es@p)

@script:python@
p << r.p;
@@

print p