// test x != NULL <=> NULL != x iso, and x != NULL => x iso

@r@
expression x;
expression E1,E2;
statement S2;
@@

- x = kmalloc(E1,E2)
+ x = kzalloc(E1,E2)
  ...
  if (x!=NULL) {
    ...
-   memset(x,0,E1);
    ...
  } else S2
