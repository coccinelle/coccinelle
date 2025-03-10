@r exists@
type t;
t *dev;
identifier f,fld;
@@

-dev->\(probe@fld\|remove@fld\)(...)
+c1()
... when any
-f(...)
+c2()
