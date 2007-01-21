@@
type T;
expression x;
expression E1,E2,E3;
@@

- x = (T)kmalloc(E1 * E2,E3)
+ x = kcalloc(E1,E2,E3)
  ...
- memset(x,0,E1 * E2);

@@
type T;
expression x;
expression E1,E2;
@@

- x = (T)kmalloc(E1,E2)
+ x = kzalloc(E1,E2)
  ...
(
- memset(x,0,E1);
|
- memset(x,0,sizeof(*x));
)
