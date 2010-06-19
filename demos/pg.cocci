@initialize:ocaml@
open Postgresql

@r@
expression x,a;
@@

f(x,a)

@script:ocaml@
y << r.x;
yy << r.a;
@@
()

@finalize:ocaml@
()