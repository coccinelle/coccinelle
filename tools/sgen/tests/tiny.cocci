@some_rule@
int e;
identifier i;
@@

  int i = e;
+ if (i < 0) i = 0;
  ...
