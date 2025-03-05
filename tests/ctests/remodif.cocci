@@
type X;
expression var,action;
@@

 var = action(...,
              sizeof(
-                    X
+                    *var
                    ),
              ...
             )
