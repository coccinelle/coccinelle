@@
type T, T2;
expression x;
expression E1,E2;
@@

- x = (T)kmalloc(E1,E2)
+ x = kzalloc(E1, E2)
  ...
(
- memset((T2)x,0,E1);
|
- memset((T2)x,0,sizeof(*x));
)
