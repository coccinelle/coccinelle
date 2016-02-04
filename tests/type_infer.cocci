@ disable all @
expression *xx;
@@

- foo(xx);

@ disable all @
type T1;
identifier x;
expression E1;
@@

- T1 x = kmalloc(E1);
  ...
- memset(E1);

