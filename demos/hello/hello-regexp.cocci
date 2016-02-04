@main@
type T;
parameter list P;
symbol printf;
expression E;
identifier M;
@@

T M(P) {
  printf(E);
  ...
}

@@
expression main.E;
@@

-  E
+  "world, hello!"