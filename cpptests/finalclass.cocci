# spatch --c++
@identifier@
identifier x;
symbol y;
@@

-x
+y
