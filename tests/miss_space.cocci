@@ expression reg, mask, val; @@

-reg &= ~mask;
-reg |= FIELD_PREP
+       FIELD_MODIFY
                   (mask,
+                   &reg,
                    val
                   );
