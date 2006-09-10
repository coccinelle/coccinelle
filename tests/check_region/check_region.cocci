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
statement S;
@@

// could an iso handle the release_region before continue?  but don't want it
// to match the continue in the then branch of check_region
probe(...) {
  ...
  for(...; ...; ...) {
    ...
(
-   x = check_region(E, req_reg_arg2);
+   x = request_region(E, req_reg_arg2, req_reg_arg3);
    ...
-   if (x) S
+   if (!x) S
|
-   if (check_region(E, req_reg_arg2)) S
+   if (!request_region(E, req_reg_arg2, req_reg_arg3)) S
)
    <...
+    release_region(E, req_reg_arg2);
     continue;
    ...>
+   release_region(E, req_reg_arg2);
  }
  ...
}

@@
@@

   ...
   probe(...)
   <...
   { ...
+    release_region(req_reg_arg1, req_reg_arg2);
     return ...;
   }
   ...>
-  request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3);
   ...

// --------------------------------------------------------------------
// intraprocedural case
// --------------------------------------------------------------------

@@
statement S;
expression new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3;
identifier x;
@@

 for(...; ...; ...) {
    ...
(
-   x = check_region(new_req_reg_arg1, new_req_reg_arg2);
+   x = request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3);
    ...
-   if (x) { continue; }
+   if (!x) { continue; }
|
-   if (check_region(new_req_reg_arg1, new_req_reg_arg2))
+   if (!request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3))
	S
)
    <...
+    release_region(new_req_reg_arg1, new_req_reg_arg2);
     continue;
    ...>
+   release_region(new_req_reg_arg1, new_req_reg_arg2);
 }
    <...
    { ...
+    release_region(req_reg_arg1, req_reg_arg2);
     return ...;
    }
    ...>
-   request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3);

// --------------------------------------------------------------------
// another intraprocedural case
// --------------------------------------------------------------------

@@
statement S;
expression new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3;
identifier x;
@@

(
-   x = check_region(new_req_reg_arg1, new_req_reg_arg2);
+   x = request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3);
    ...
-   if (x)
+   if (!x)
       { return ...; }
|
-   if (check_region(new_req_reg_arg1, new_req_reg_arg2))
+   if (!request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3))
       { return ...; }
)
    <...
    { ...
+     release_region(new_req_reg_arg1, new_req_reg_arg2);
      return ...;
    }
    ...>
-   request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3);
