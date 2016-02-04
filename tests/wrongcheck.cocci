@r@
expression e1,e2;
identifier f;
statement S1,S2;
@@

 e1 = f(...,
-e2
+blah
  ,...);
if (e2 == NULL || ...) S1 else S2

@depends on !r@
expression e1,e2;
@@

- e1 = e2;
