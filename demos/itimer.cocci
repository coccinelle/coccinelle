@@
@@

-      if (cputime_eq(cputime, cputime_zero))
-	  cputime = jiffies_to_cputime(1);
+      if (cputime_gt(cputime, cputime_zero))
+         cputime = cputime_add(cputime,
+                                       jiffies_to_cputime(1));
