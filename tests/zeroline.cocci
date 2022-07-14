@@
type T;
symbol x;
@@
- T x;
+ T x = { 0 };
  ...
- memset(&x, 0, ...);

@@
type T;
symbol y;
@@
- T y;
+ T y = { 0,4 };
  ...
- memset(&y, 0, ...);
