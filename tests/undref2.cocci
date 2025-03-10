virtual xxx

@r depends on xxx@
identifier f;
position p;
@@

f@p();

@@
identifier f;
position p != r.p;
@@

-f@p()
+42

@script:ocaml@
f << r.f = "unknown";
@@

Printf.printf "the value of f: %s\n" f
