@r@
identifier x;
@@

f(x);

@@
fresh identifier a = "foo-" ## r.x;
@@

- g();
+ g(a);
