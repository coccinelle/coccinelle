@r@
type T;
T:b x;
@@

f(x)

@script:python depends on r@
b << r.b;
@@
assert (b == "4")

@@
int:5 x;
@@

-f(x)
+h(x)

@@
int:4 x;
@@

-f(x)
+g(x)
