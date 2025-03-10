# spatch --c++
@@
expression e;
@@

- e();
+ e(0);
