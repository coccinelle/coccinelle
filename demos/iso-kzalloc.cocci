@@
expression E;
constant c;
type T;
@@

-kzalloc(c * sizeof(T), E)
+kcalloc(c, sizeof(T), E)
