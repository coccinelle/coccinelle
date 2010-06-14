@r@
expression x;
@@

f(x)

@script:ocaml@
y << r.x;
@@

Printf.printf "%s\n" y

@script:ocaml@
y << r.x;
@@

Printf.printf "%s again\n" y
