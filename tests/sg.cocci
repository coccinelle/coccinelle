@@
@@

scoped_guard(...)
  {
   ... when any
   if (...) {
      ...
-     return ...;
+     x = 12;
   }
   ... when any
  }
