@initialize:ocaml@
@@

let x = 0

@initialize:ocaml@
@@

let y = 1

@initialize:python@
@@

x = 0

@initialize:python@
@@

y = 1

@@
@@
- return 0;
+ return 1;

@finalize:ocaml@
@@
assert (x = 0)

@finalize:ocaml@
@@
assert (y = 1)

@finalize:python@
@@
assert x == 0

@finalize:python@
@@
assert y == 1