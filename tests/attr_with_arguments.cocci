@@
attribute name aarg;
@@

 aarg(1,2)
-int
+char
 func()
 { ... }

@@
attribute name aarg;
@@

 aarg("not (1,2)")
-int
+long
 func()
 { ... }

@@
attribute name aarg2;
@@

 aarg2(...)
-int
+short
 func()
 { ... }

@@
attribute name aarg3;
@@
-int
+long
 id aarg3(1,2);

@@
attribute name aarg4;
@@
-void
+long
 func() aarg4(1,2) {
 }
