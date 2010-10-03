@@
identifier x;
expression E, E1, E2;
@@

main(int x) {
  <...
   {
   ...
-  x();
   ...
-  if (f()) return E1;
   <...
-   g(E)
+   h(E)
    ...>
?- if (i()) return E2;
   ...
   }
  ...>
}