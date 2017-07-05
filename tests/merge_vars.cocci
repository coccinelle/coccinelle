@initialize:ocaml@
@@

let v1 = 1
let v2 = 2
let v3 = 3
let v4 = 4
let v5 = 5

@@
@@
-f
+f

@finalize:ocaml@
l1 << merge.v1;
l2 << merge.v2;
l3 << merge.v3;
@@

assert (l1 = [1]);
assert (l2 = [2]);
assert (l3 = [3])

@finalize:ocaml@
l1 << merge.v4;
l2 << merge.v5;
@@

assert (l1 = [4]);
assert (l2 = [5])