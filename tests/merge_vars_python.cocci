@initialize:python@
@@

coccinelle.v1 = 1
coccinelle.v2 = 2
coccinelle.v3 = 3
coccinelle.v4 = 4
coccinelle.v5 = 5

@@
identifier f;
@@
-f
+f

@finalize:python@
l1 << merge.v1;
l2 << merge.v2;
l3 << merge.v3;
@@
assert (l1 == [1])
assert (l2 == [2])
assert (l3 == [3])


@finalize:python@
l1 << merge.v4;
l2 << merge.v5;
@@
assert (l1 == [4])
assert (l2 == [5])