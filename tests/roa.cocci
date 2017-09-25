@@
identifier i,j;
attribute name __ro_after_init;
@@

struct
- i
+ found
     j __ro_after_init =
 { ... };

@disable optional_attributes@
identifier i,j;
@@

struct i
-     j
+     not_used
    =
 { ... };

@@
identifier i,j;
@@

struct i
-     j
+     used
    =
 { ... };

