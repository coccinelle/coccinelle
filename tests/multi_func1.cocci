@@
identifier fn1, fn2, fn3;
expression A;
@@

  fn1(...) {
-   foo(A);
  }

  fn2(...) {
-   bar(A);
  }

  fn3(...) {
    fn1(...);
    fn2(...);
  }
