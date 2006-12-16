@@
identifier x;
expression E;
type T;
@@

(
free(x);
|
kfree(x);
)
... WHEN != T x = E;
x

@@
identifier x;
expression E;
@@


xfree(x);
... WHEN != x = E;
x
