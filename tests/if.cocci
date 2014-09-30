@exists@
expression x;
@@

f(x);
+ after();
 ... when != true x == NULL || ...
g(x);
+ after();

@exists@
expression x;
@@

+before();
f(x);
 ... when != false x == NULL || ...
+before();
g(x);
