@@
expression E, req_reg_arg2;
identifier probe;
@@

probe(...) {
  ...
  for(...; ...; ...) {
    ...
+   if (!request_region(E, req_reg_arg2, req_reg_arg3))
-   if (check_region(E, req_reg_arg2))
      { continue; }
    <...
+    release_region(E, req_reg_arg2);
     continue;
    ...>
+   release_region(E, req_reg_arg2);
  }
  ...
}
