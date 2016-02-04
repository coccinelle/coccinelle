@ voidfunc @
function fn;
position voidpos;
@@
  void  fn@voidpos(...) {
    ...
  }
    
@ func disable ret exists @
type T;
function fn;
position pos != voidfunc.voidpos;
@@
  T  
-    fn@pos
+    newname
     (...) {
    ... WHEN != return ...;
  }
