@@
expression E;
identifier x;
@@

  f(...) {
+   spin_lock();
    ...
+   spin_unlock();
  }