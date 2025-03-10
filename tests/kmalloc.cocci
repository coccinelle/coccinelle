@@
expression x;
expression E1,E2;
type T;
@@

  x =
-   (T)kmalloc(E1,E2)
+   kzalloc(E1, E2)
  ...
- memset(x,0,E1);