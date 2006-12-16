@@
identifier x;
expression E;
@@

(
free(x);
|
kfree(x);
)
... WHEN != x = E;
x

@@
identifier x;
expression E;
@@


xfree(x);
... WHEN != x = E;
x
