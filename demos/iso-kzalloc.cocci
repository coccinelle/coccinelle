// A*B is not just a toy isomorphism :) it's really useful sometimes.

@@
expression E;
constant c;
type T;
@@

-kzalloc(c * sizeof(T), E)
+kcalloc(c, sizeof(T), E)
