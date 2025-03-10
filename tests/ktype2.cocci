@r@
type T, T2;
type T1;
T1 *x;
T1 *y;
expression E2;
@@

- x = kmalloc(sizeof(T1),E2)
+ x = kzalloc(sizeof(T1), E2)
  ...
- memset(x,0,sizeof(*y));
