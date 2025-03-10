@r@
expression e;
position p;
@@

e@p

@script:ocaml@
e << r.e;
p << r.p;
@@

Printf.printf "%s: %s: %d\n" e (List.hd p).file (List.hd p).line

