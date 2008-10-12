@exists@
expression x;
statement S1,S2;
@@

f(x);
+ after();
 ... when != true x == NULL || ...
g(x);
+ after();

@exists@
expression x;
statement S1,S2;
@@

+before();
f(x);
 ... when != false x == NULL || ...
+before();
g(x);
