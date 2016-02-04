// iso X * Y will handle the variations 

@@
expression E;
constant c;
type T;
@@

- kzalloc(sizeof(T) * c, E)
+ kcalloc(c, sizeof(T), E)
