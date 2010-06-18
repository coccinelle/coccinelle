@r@
expression x,a;
@@

f(x,a)

@script:ocaml@
y << r.x;
yy << r.a;
@@

Printf.printf "%s and %s\n" y yy

@script:ocaml@
y << r.a;
zz << r.x;
@@

Printf.printf "%s again and %s again \n" zz y
