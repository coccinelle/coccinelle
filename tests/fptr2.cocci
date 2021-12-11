@r@
type T;
T e;
expression f;
@@

f(e)

@script:ocaml r1@
t << r.T;
x;
@@

if t = "int ( * ) ( int c , int d )"
then x := make_ident "success"
else x := make_ident "failure"

@@
type r.T;
T r.e;
expression r.f;
identifier r1.x;
@@

- f(e)
+ x
