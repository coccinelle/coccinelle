@r@
identifier i;
pragmainfo p;
@@

#pragma i p

@script:ocaml s@
p << r.p;
p2;
@@

p2 := Coccilib.make_pragmainfo (String.concat " " (List.tl (String.split_on_char ' ' p)))

@@
identifier r.i;
pragmainfo r.p,s.p2;
@@

- #pragma i p
+ #pragma i p2

@r2@
identifier i;
pragmainfo p;
@@

#pragma i p

@script:python s2@
p << r2.p;
p3;
@@

coccinelle.p3 = cocci.make_pragmainfo ("pythoninfo " + p)

@@
identifier r2.i;
pragmainfo r2.p,s2.p3;
@@

- #pragma i p
+ #pragma i p3
