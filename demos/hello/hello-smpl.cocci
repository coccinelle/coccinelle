@main@
type T;
parameter list P;
symbol printf;
expression E;
@@

T main(P) {
  printf(E);
  ...
}

@@
expression main.E;
@@

-  E
+  "world, hello!"