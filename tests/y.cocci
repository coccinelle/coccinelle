@ rule1 @
expression E;
@@

+ xxx(E);
  foo(E);

@@
expression F;
@@

  xxx(F);

@@
expression rule1.E;
@@

  foo(E);
+ yyy(E);
