@r@
identifier x;
@@

-f(x);
+g(x);

@script:ocaml@
x << r.x;
@@

if x = "done" then
  Coccilib.exit()

@@
identifier x;
@@

g(x
+ ,y
 );
