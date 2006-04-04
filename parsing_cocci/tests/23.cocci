@@
expression X, Y, E;
identifier field;
error erry;
@@

- Y = init_etherdev(NULL,X);
+ Y = alloc_etherdev(X);
  ...
  Y.field = E;
+ if (!register_netdev(Y)) {
+   kfree(Y);
+   return erry;
+ }

@@
error errx;
@@

  Y = alloc_etherdev(X);
  <...
  if (...) {
    ...
-   unregister_netdev(Y)
    ...
    return errx;
  }
  ...>
  register_netdev(Y)
