@script:ocaml r@
foo;
rv;
stm;
stm1;
unbound;
@@

foo := Coccilib.make_ident "one";
rv := Coccilib.make_expr "x < 0";
stm := Coccilib.make_stmt "if (c < 0) return 12;";
stm1 := Coccilib.make_stmt_with_env "int c;" "if (c) return 12;"

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
