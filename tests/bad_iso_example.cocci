@r@
expression E,E1;
statement S1,S2;
@@

+ E = E1;
if (
-   !(E = E1)
+   E
   )
S1 else S2

@@
expression E,E1;
statement S1,S2;
@@

+ E = E1;
if (
-   E = E1
+   E
   )
S1 else S2
