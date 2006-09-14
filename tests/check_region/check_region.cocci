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
-   if (x)
+   if (!x)
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
|
-   if (check_region(E, req_reg_arg2))
+   if (!request_region(E, req_reg_arg2, req_reg_arg3))
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
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
(
+    release_region(req_reg_arg1, req_reg_arg2);
     return ...;
|
+    release_region(req_reg_arg1, req_reg_arg2);
     return;
)
   }
   ...>
-  request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3);
   ...

// --------------------------------------------------------------------
// intraprocedural case
// --------------------------------------------------------------------

@@
expression new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3;
identifier x;
@@

 for(...; ...; ...) {
    ...
(
-   x = check_region(new_req_reg_arg1, new_req_reg_arg2);
+   x = request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3);
    ...
-   if (x)
+   if (!x)
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
|
-   if (check_region(new_req_reg_arg1, new_req_reg_arg2))
+   if (!request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3))
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
)
    <...
+    release_region(new_req_reg_arg1, new_req_reg_arg2);
     continue;
    ...>
+   release_region(new_req_reg_arg1, new_req_reg_arg2);
 }
    <...
(
    { ...
+    release_region(new_req_reg_arg1, new_req_reg_arg2);
     return ...;
    }
|
    { ...
+    release_region(new_req_reg_arg1, new_req_reg_arg2);
     return;
    }
)
    ...>
-   request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3);

// --------------------------------------------------------------------
// another intraprocedural case
// --------------------------------------------------------------------

@@
expression new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3;
identifier x;
@@

(
-   x = check_region(new_req_reg_arg1, new_req_reg_arg2);
+   x = request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3);
    ...
-   if (x)
+   if (!x)
(
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
|
       { ... when = \( printk(...); \| dbg(...); \)
	return; }
)
|
-   if (check_region(new_req_reg_arg1, new_req_reg_arg2))
+   if (!request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3))
(
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
|
       { ... when = \( printk(...); \| dbg(...); \)
	return; }
)
)
    <...
    { ...
(
+     release_region(new_req_reg_arg1, new_req_reg_arg2);
      return ...;
|
+     release_region(new_req_reg_arg1, new_req_reg_arg2);
      return;
)
    }
    ...>
-   request_region(new_req_reg_arg1, new_req_reg_arg2, new_req_reg_arg3);

//// --------------------------------------------------------------------
//// interprocedural case
//// --------------------------------------------------------------------
//
// the following doesn't work, because the third rule doesn't make any
// constraints.  it would if it were put first, but then it would match
// alomost everything
//
//@@
//identifier config;
//expression req_reg_arg1, req_reg_arg2, req_reg_arg3;
//@@
//config(...) {
//  ...
//-   request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3)
//  ...
//}
//
//@@
//identifier probe;
//identifier x;
//expression E;
//@@
//
//// could an iso handle the release_region before continue?  but don't want it
//// to match the continue in the then branch of check_region
//probe(...) {
//  ...
//(
//-   x = check_region(E, req_reg_arg2);
//+   x = request_region(E, req_reg_arg2, req_reg_arg3);
//    ...
//-   if (x)
//+   if (!x)
//      { ... when = \( printk(...); \| dbg(...); \)
//	return ...; }
//|
//-   if (check_region(E, req_reg_arg2))
//+   if (!request_region(E, req_reg_arg2, req_reg_arg3))
//      { ... when = \( printk(...); \| dbg(...); \)
//	return ...; }
//)
//    <...
//    if (...) {
//      ...
//+     release_region(E, req_reg_arg2);
//      return ...;
//    }
//    ...>
//  }
//
//@@
//@@
//
//   ...
//   probe(...)
//   ...
//   config(...)
//   ...
// --------------------------------------------------------------------
// interprocedural case
// --------------------------------------------------------------------

@@
identifier config;
expression req_reg_arg1, req_reg_arg3;
expression E;
@@
  config(...) {
    ...
-    request_region(req_reg_arg1, E, req_reg_arg3)
    ...
  }

@@
identifier probe;
identifier x;
expression E;
expression req_reg_arg2;
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
-   if (x)
+   if (!x)
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
|
-   if (check_region(E, req_reg_arg2))
+   if (!request_region(E, req_reg_arg2, req_reg_arg3))
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
)
    <...
+    release_region(E, req_reg_arg2);
     continue;
    ...>
(
    if(config(...)) { return ...; }
|
    if(!config(...)) { return ...; }
)
+   release_region(E, req_reg_arg2);
  }
  ...
}

@@
identifier config;
@@
  config(...) {
    ...
-   request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3)
    ...
  }
