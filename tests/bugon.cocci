@@ expression E; @@

- if (unlikely(E)) { BUG(); }
+ BUG_ON(E);
