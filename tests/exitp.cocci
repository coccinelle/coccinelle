@r@
identifier x;
@@

-f(x);
+g(x);

@script:python@
x << r.x;
@@

if ("%s" % x) == "done":
  cocci.exit()

@@
identifier x;
@@

g(x
+ ,y
 );
