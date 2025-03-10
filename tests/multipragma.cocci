@r@
position p;
@@

return;@p

@script:ocaml@
p << r.p;
@@

Printf.printf "line: %d\n" (List.hd p).line
