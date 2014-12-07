@ rule1 disable add_signed @
parameter list[n] P;
identifier x;
@@

f (P,int x,...) { ... }

@ rule3 disable add_signed @
expression list[rule1.n] Es;
expression E;
@@

g (Es,
-    E,
   ...)

@ rule2 disable add_signed @
parameter list[rule1.n] P;
identifier x;
@@

g (P,
-    int x,
   ...) { ... }
