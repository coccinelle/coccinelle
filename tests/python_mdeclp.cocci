@script:python r@
p;
@@

coccinelle.p = cocci.make_position("tests/python_mdeclp.c", "one", 1, 4, 1, 7)

@@
position r.p;
identifier f;
@@

- f@p(...) { ... }
