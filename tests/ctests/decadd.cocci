@@
declarer name DECL;
attribute name __cb;
identifier x;
symbol y;
@@

struct x {
  ...
  DECL(x)
+ __cb(y)
  ;
  ...
}
