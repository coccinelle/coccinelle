// Based on request from Michael D. Day

@@
expression X, Y;
@@

- kcalloc(1, X, Y)
+ kmalloc(X, Y)
