@a@
identifier x;
@@

foo(x);

@script:ocaml b@
x << a.x;
y;
z;
@@

y := make_ident x;
z := make_ident "something"

@c@
identifier b.y;
identifier b.z;
identifier a.x;
@@

- bar();
+ matched_bar(y,z,x);
