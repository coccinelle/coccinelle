// --------------------------------------------------------------------
// interprocedural case
// --------------------------------------------------------------------

@@
expression req_reg_arg1, req_reg_arg2, req_reg_arg3;
@@
    request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3)

@@
identifier probe;
identifier x;
expression E;
@@
// interprocedural case

probe(...) {
  ...
//  for(...; ...; ...) {
//    ...
-   if (check_region(E, req_reg_arg2)) continue;
+   if (!request_region(E, req_reg_arg2, req_reg_arg3)) continue;
    <...
+    release_region(E, req_reg_arg2);
     continue;
    ...>
+   release_region(E, req_reg_arg2);
//  }
  ...
}

//@@
//@@
//
//   ...
//   probe(...)
//   <...
//   { ...
//+    release_region(req_reg_arg1, req_reg_arg2);
//     return ...;
//   }
//   ...>
// - request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3)
//   ...