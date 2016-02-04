@@ expression current; @@

-  current->flags & PF_FREEZE
+  freezing(current)
   ...
?- refrigerator(PF_FREEZE)
+  refrigerator()
   ...
?  current->flags & PF_FREEZE
