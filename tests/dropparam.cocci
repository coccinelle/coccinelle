@ rule1 disable add_signed @
parameter list[n] P;
identifier x;
@@

f (P,int x,...) { ... }

@ rule2 disable add_signed @
parameter list[rule1.n] P;
identifier x;
@@

g (P,
-    int x,
   ...) { ... }
