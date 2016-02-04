@initialize:ocaml@
@@
let a = ref 0
let _ = Printf.printf "starting ocaml\n"

@r@
expression x,a;
@@

f(x,a)

@script:ocaml@
y << r.x;
yy << r.a;
@@

a := !a + 1;
Printf.printf "%s and %s\n" y yy

@script:ocaml@
y << r.a;
zz << r.x;
@@

a := !a + 1;
Printf.printf "%s again and %s again \n" zz y

@finalize:ocaml@
@@
Printf.printf "ending ocaml %d\n" !a
