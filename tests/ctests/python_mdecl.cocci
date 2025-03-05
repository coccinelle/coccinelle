@script:python r@
foo;
rv;
stm;
stm1;
unbound;
@@

coccinelle.foo = cocci.make_ident("one")
coccinelle.rv = cocci.make_expr("x < 0")
coccinelle.stm = cocci.make_stmt("if (c < 0) return 12;")
coccinelle.stm1 = cocci.make_stmt_with_env("int c;", "if (c) return 12;")

@@
identifier r.foo;
expression r.rv;
statement r.stm;
expression r.unbound;
@@

- foo() { stm ... return rv; }

@@
identifier r.foo;
expression r.rv;
statement r.stm1;
@@

- foo() { ... stm1 ... return rv; }
