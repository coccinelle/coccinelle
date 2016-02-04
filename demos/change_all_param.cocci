// author: Pad. Example based on discussions with Nicholas Mc Guire.

// call site
@ r1 @
identifier fn;
expression ret;
@@

- ret =
  fn(
+   ret,
    ...)


@ r2 @
identifier r1.fn;
expression e;
@@


fn (...,
-   e
+   &e
    ,...)


// definition site
@@
type T;
identifier r1.fn;
@@

- T 
+ void
  fn(
+    T ret,
     ...)
{ 
...
}


@ rparam @
identifier r1.fn;
type T;
identifier x;
@@

fn(...,
- T x
+ T *x
  ,...)
{
...
}

// weird, if I inline this rule in previous rule it does
// not work
@@
identifier r1.fn;
identifier rparam.x;
@@ 
fn(...)
{
<...
- x
+ *x
...> 
}