@@
identifier ty,x;
expression a;
initializer list is;
@@

struct ty x = 
  {is,
- .i = a,
+ foo(a),
  ...};
