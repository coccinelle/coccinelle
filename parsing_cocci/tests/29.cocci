@@
struct tty_operations t;
local function fn;
@@

t.write = fn;

@@
parameter p;
identifier x;
@@

   fn(p,
-     int x,
      ...) {
     <***
-     x
+     0
      ***>
   }

@@
expression E1, E2;
@@

-  fn(E1, E2,
+  fn(E1,
      ...)
