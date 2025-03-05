@@
identifier func; // work with local function ?  with function ?
expression X,Y;
@@

 func(...) {
   ...
   f(X);
   ...
-  h(Y);
+  h(X, Y); 
   ...
 }
