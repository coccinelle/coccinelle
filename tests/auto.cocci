# spatch --c++

@@
identifier x;
identifier y;
@@
- y = x
+ x = y

@@
identifier x;
@@
- x = 2
+ y = 2
