@@ expression current; @@

-  current->flags & PF_FREEZE
+  freezing(current)
   ...
?- refrigerator(PF_FREEZE)
+  refrigerator()
