# spatch --c++=11

@@
type t;
identifier x, y;
@@
- type y = x;
+ type x = y;

@@
type t
identifier x;
@@
- t x = 2
+ auto y = 2
