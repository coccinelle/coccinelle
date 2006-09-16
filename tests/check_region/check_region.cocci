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
// Case 1
// --------------------------------------------------------------------

@@
expression req_reg_arg1, req_reg_arg2, req_reg_arg3;
identifier x;
@@

(
-   x = check_region(req_reg_arg1, req_reg_arg2);
+   x = request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3);
    ...
-   if (x)
+   if (!x)
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
|
-   if (check_region(req_reg_arg1, req_reg_arg2))
+   if (!request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3))
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
)
    <...
+   release_region(req_reg_arg1, req_reg_arg2);
    return ...;
    ...>
-   request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3);


// --------------------------------------------------------------------
// Case 2
// --------------------------------------------------------------------

@@
expression req_reg_arg1, req_reg_arg2, req_reg_arg3;
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
-   if (check_region(req_reg_arg1, req_reg_arg2))
+   if (!request_region(req_reg_arg1, req_reg_arg2, req_reg_arg3))
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

// --------------------------------------------------------------------
// Case 3
// --------------------------------------------------------------------

@@
expression req3_reg_arg1, req3_reg_arg2, req3_reg_arg3;
expression E;
identifier x;
identifier fn1, fn2, fn3;
statement S;
@@

fn2(...) {
    <...
+     release_region(req3_reg_arg1, req3_reg_arg2);
      return ...;
    ...>
-   request_region(req3_reg_arg1, req3_reg_arg2, req3_reg_arg3);
    ...
}

fn1(...) {
  ...
(
-   x = check_region(E, req3_reg_arg2);
+   x = request_region(E, req3_reg_arg2, req3_reg_arg3);
    ...
-   if (x)
+   if (!x)
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
|
-   if (check_region(E, req3_reg_arg2))
+   if (!request_region(E, req3_reg_arg2, req3_reg_arg3))
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
)
  ...
}

fn3(...) {
  ...
  if (\(fn1(...) \| !fn1(...) \)) S
  ...
  fn2(...)
  ...
}

// --------------------------------------------------------------------
// Case 4
// --------------------------------------------------------------------

@@
expression req4_reg_arg1, req4_reg_arg2, req4_reg_arg3;
expression E;
identifier x;
identifier fn1, fn2, fn3;
statement S;
@@

fn2(...) {
    <...
+     release_region(req4_reg_arg1, req4_reg_arg2);
      return ...;
    ...>
-   request_region(req4_reg_arg1, req4_reg_arg2, req4_reg_arg3);
    ...
}

fn1(...) {
 ...
 for(...; ...; ...) {
    ...
(
-   x = check_region(E, req4_reg_arg2);
+   x = request_region(E, req4_reg_arg2, req4_reg_arg3);
    ...
-   if (x)
+   if (!x)
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
|
-   if (check_region(E, req4_reg_arg2))
+   if (!request_region(E, req4_reg_arg2, req4_reg_arg3))
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
)
    <...
+    release_region(E, req4_reg_arg2);
     continue;
    ...>
+   release_region(E, req4_reg_arg2);
 }
...
}

fn3(...) {
  ...
  if (\(fn1(...) \| !fn1(...) \)) S
  ...
  fn2(...)
  ...
}

// --------------------------------------------------------------------
// Case 5
// --------------------------------------------------------------------

@@
expression req5_reg_arg1, req5_reg_arg2, req5_reg_arg3;
expression E;
identifier x;
identifier fn1, fn2;
@@

 fn2(...) {
    <...
+     release_region(req5_reg_arg1, req5_reg_arg2);
      return ...;
    ...>
-   request_region(req5_reg_arg1, req5_reg_arg2, req5_reg_arg3);
    ...
 }

fn1(...) {
 ...
(
-   x = check_region(E, req5_reg_arg2);
+   x = request_region(E, req5_reg_arg2, req5_reg_arg3);
    ...
-   if (x)
+   if (!x)
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
|
-   if (check_region(E, req5_reg_arg2))
+   if (!request_region(E, req5_reg_arg2, req5_reg_arg3))
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
)
    <...
+     release_region(req5_reg_arg1, req5_reg_arg2);
      return ...;
    ...>
-   request_region(req5_reg_arg1, req5_reg_arg2, req5_reg_arg3);
 ...
 fn2(...)
 ...
}

// --------------------------------------------------------------------
// Case 6
// --------------------------------------------------------------------

@@
expression req6_reg_arg1, req6_reg_arg2, req6_reg_arg3;
expression E;
identifier x;
identifier fn1, fn2;
@@

fn2(...) {
    <...
+     release_region(req6_reg_arg1, req6_reg_arg2);
      return ...;
    ...>
-   request_region(req6_reg_arg1, req6_reg_arg2, req6_reg_arg3);
    ...
}

fn1(...) {
 ...
 for(...; ...; ...) {
    ...
(
-   x = check_region(E, req6_reg_arg2);
+   x = request_region(E, req6_reg_arg2, req6_reg_arg3);
    ...
-   if (x)
+   if (!x)
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
|
-   if (check_region(E, req6_reg_arg2))
+   if (!request_region(E, req6_reg_arg2, req6_reg_arg3))
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
)
    <...
+    release_region(E, req6_reg_arg2);
     continue;
    ...>
+   release_region(E, req6_reg_arg2);
 }
 ...
 fn2(...)
 ...
}

// --------------------------------------------------------------------
// Case 7
// --------------------------------------------------------------------

@@
expression req7_reg_arg1, req7_reg_arg2, req7_reg_arg3;
expression E;
identifier x;
identifier fn1, fn2;
@@

fn1(...) {
 ...
(
   x = check_region(E, req7_reg_arg2);
    ...
   if (x)
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
|
   if (check_region(E, req7_reg_arg2))
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
)
 ...
}

fn2(...) {
    ...
    if (\(fn1(...) \| !fn1(...) \))
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
    <...
+     release_region(req7_reg_arg1, req7_reg_arg2);
      return ...;
    ...>
-   request_region(req7_reg_arg1, req7_reg_arg2, req7_reg_arg3);
    ...
}

fn1(...) {
 ...
(
-   x = check_region(E, req7_reg_arg2);
+   x = request_region(E, req7_reg_arg2, req7_reg_arg3);
    ...
-   if (x)
+   if (!x)
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
|
-   if (check_region(E, req7_reg_arg2))
+   if (!request_region(E, req7_reg_arg2, req7_reg_arg3))
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
)
 ...
}

// --------------------------------------------------------------------
// Case 8
// --------------------------------------------------------------------

@@
expression req8_reg_arg1, req8_reg_arg2, req8_reg_arg3;
expression E;
identifier x;
identifier fn1, fn2;
@@

fn1(...) {
 ...
 for(...; ...; ...) {
    ...
(
   x = check_region(E, req8_reg_arg2);
    ...
   if (x)
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
|
   if (check_region(E, req8_reg_arg2))
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
)
    ...
 }
...
}

fn2(...) {
    ...
    if (\(fn1(...) \| !fn1(...) \))
       { ... when = \( printk(...); \| dbg(...); \)
	return ...; }
    <...
+   release_region(req8_reg_arg1, req8_reg_arg2);
    return ...;
    ...>
-   request_region(req8_reg_arg1, req8_reg_arg2, req8_reg_arg3);
    ...
}

fn1(...) {
 ...
 for(...; ...; ...) {
    ...
(
-   x = check_region(E, req8_reg_arg2);
+   x = request_region(E, req8_reg_arg2, req8_reg_arg3);
    ...
-   if (x)
+   if (!x)
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
|
-   if (check_region(E, req8_reg_arg2))
+   if (!request_region(E, req8_reg_arg2, req8_reg_arg3))
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
)
    <...
+    release_region(E, req8_reg_arg2);
     continue;
    ...>
+   release_region(E, req8_reg_arg2);
 }
...
}
