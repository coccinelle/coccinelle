// Cases:
// 1. both in same function, no loop
// 2. both in same function, loop on check_region
// 3. check in fn1 no loop, request in fn2, fn3 calls fn1 then fn2
// 4. check in fn1 with loop, request in fn2, fn3 calls fn1 then fn2
// 5. check in fn1 no loop, request in fn2, fn1 calls fn2
// 6. check in fn1 with loop, request in fn2, fn1 calls fn2
// 7. check in fn1 no loop, request in fn2, fn2 calls fn1
// 8. check in fn1 with loop, request in fn2, fn2 calls fn1

// --------------------------------------------------------------------
// Case 2
// --------------------------------------------------------------------

@@
expression req_reg_arg1, req_reg_arg2, req_reg_arg3, E;
identifier x;
@@

 for(...; ...; ...) {
    ...
(
-   x = check_region(req_reg_arg1, req_reg_arg2);
+   x = request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3);
    ...
-   if (x)
+   if (!x)
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
|
-   if (check_region(req_reg_arg1, req_reg_arg2) != 0)
+   if (!request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3))
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
|
-   if (check_region(req_reg_arg1, req_reg_arg2) != 0 && E)
+   if (!request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3) && E)
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
)
    <...
+    release_region(req_reg_arg1, req_reg_arg2);
     continue;
    ...>
+   release_region(req_reg_arg1, req_reg_arg2);
 }
    <...
+   release_region(req_reg_arg1, req_reg_arg2);
    return ...;
    ...>
-   request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3);
