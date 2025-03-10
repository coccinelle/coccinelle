@@
expression x,e;
identifier y;
symbol q;
@@

(
x = kzalloc(...,e|__GFP_NOFAIL);
|
x = kzalloc(...);
...
(
x == NULL
|
x != NULL
|
-x->y
+x->q
)
//... when any
)