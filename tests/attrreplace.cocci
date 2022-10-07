@@
attribute name __att1, __att2, __att3;
@@

// asttoctl doesn't allow adding a function after a removed one
// search for let function_header
+ int __att3 * __att2 * __att1 reversed() { bbb(0); }
- int __att1 * __att2 * __att3 foo() { aaa(0); }

@@
@@

 int
- __att1
 *
- __att2
 *
- __att3
 bar() { return 0; }

@@
@@

+ __att1
 int
 *
+ __att2
 *
+ __att3
 xyzbefore() { return 0; }

@@
@@

 int
+ __att1
 *
+ __att2
 *
+ __att3
 xyzafter() { return 0; }
