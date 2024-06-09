#spatch --c++

@@
expression e1,e2;
@@

- e1 > e2
+ 12
