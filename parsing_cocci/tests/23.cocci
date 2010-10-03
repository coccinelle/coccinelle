@@
expression X, Y, E;
identifier field;
error err;
@@

- Y = init_etherdev(NULL,X);
+ Y = alloc_etherdev(X);
  ...
  Y.field = E;
+ if (!register_netdev(Y)) {
+   kfree(Y);
+   return err;
+ }

@@
error err;
@@

  Y = alloc_etherdev(X);
  <...
  if (...) {
    ...
-   unregister_netdev(Y)
    ...
    return err;
  }
  ...>
  ...
  register_netdev(Y)
  ...
