@ rule1 @
expression X,Y,Z;
@@
   f(X);
   ... when any, strict
   g(Z);
   ... when any, strict
-  h(Y);
+  h(X,Y,Z); 
