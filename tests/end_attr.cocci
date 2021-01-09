@@
type T;
identifier var;
attribute name autofree;
@@

   T var
-  autofree
   ;
   ...
++ free(var);
   return ...;

@@
type T;
identifier var;
attribute name autofree;
@@

   T var
-  autofree
   = NULL;
   ...
++ free(var);
   return ...;
