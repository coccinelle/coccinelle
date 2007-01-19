@@
expression x;
expression E;
expression f;
@@

  free(x);
  ... WHEN != x = E
+ printf("possible use after free!!\n");
  f(...,x,...);
