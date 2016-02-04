@@
identifier ty,x;
expression a;
initializer list is;
@@

struct ty x[] = {...,
  {is,
- a
+ .i = foo(a)
  ,...}, ...};
