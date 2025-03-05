@ rule1 @
expression X,Y,Z;
@@
   f(X);
   ...
   g(Z);
   ...
-  h(Y);
+  h(X, Y, Z); 
