@r@
statement list sl;
identifier f;
@@

f() {
 sl
}

@script:ocaml s@
sl << r.sl;
x;
@@

x := Coccilib.make_stmt(List.hd (String.split_on_char '\n' sl))

@@
identifier f;
statement s.x;
@@

f() {
+ x
...
}