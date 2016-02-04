@a@
identifier x;
@@

foo(x);

@script:ocaml b@
x << a.x;
y;
z;
@@

y := x;
z := "something"

@c@
identifier b.y;
identifier b.z;
identifier a.x;
@@

- bar();
+ matched_bar(y,z,x);
