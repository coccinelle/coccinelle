virtual xxx

@r depends on xxx@
identifier f;
position p,p1;
@@

f@p1();

@@
identifier f;
position p != r.p;
position r.p1;
@@

(
-f@p1()
+22
|
-f@p()
+42
)

@script:ocaml@
f << r.f = "unknown";
@@

Printf.printf "the value of f: %s\n" f
