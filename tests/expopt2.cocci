@@
identifier v,w;
identifier fld;
@@
 f(v,
(
-    v.fld
+    v->fld
|
-    w.aa
+    g(w.aa)
)
 )
