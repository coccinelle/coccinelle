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
... WHEN != \(T x = E; \| x = E;\)
x
