@@
identifier x;
expression E1, E2, E3;
@@

-  x = request_region(E1,E2,E3);
  <...
-   if (...) {
-     ... when != release_region(E1,E2);
-     return ...;
-   }
  ...>
?- release_region(E1,E2);
