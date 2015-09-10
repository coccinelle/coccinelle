@@
identifier f;
declarer name NOBUG;
@@

  f(...) { ... }
+ NOBUG();

@@
identifier f;
declarer name BUG;
@@

  f(...) { ... }
+ static BUG();
