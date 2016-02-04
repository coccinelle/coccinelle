// gives already tagged token, because E can bind in two different ways
// so both substitutions are returned.
@@
expression E;
statement S;
@@

if (
(
-    foo() && (E)
+    E
|
-    foo() && E
+    E
)
   ) S
