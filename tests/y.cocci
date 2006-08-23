@@
expression E;
@@

+ xxx(E);
  foo(E);

@@
expression F;
@@

  bar(F);
+ ddd(); // if this is dropped, the next rule is not applied

@@
@@

  foo(E);
+ yyy(E);
