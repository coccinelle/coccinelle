@ rule1 @
parameter list[n] P;
identifier x;
@@

f (P,int x,...) { ... }

@@
parameter list[rule1.n] P;
identifier x;
@@

g (P,
-    int x,
   ...) { ... }
