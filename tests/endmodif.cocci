@@ @@
- f (void) { ... }

@@ @@
- g (void) { ... }
+ replace_g(void) { return 12; }

@@ @@
+ before_h(void) { return 12; }
  h (void) { ... }
+ after_h(void) { return 12; }

@@ @@
  i (void) { ... }
+ after_i1(void) { return 12; }
+ after_i2(void) { return 12; }
