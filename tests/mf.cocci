@@
identifier fn1, fn2;
expression A, B;
@@

fn1(...) {
  foo(A);
}

fn2(...) {
  fn1();
- bar(B);
+ xxx(A);
}

@@
@@

fn1(...) {
- foo(A);
}
