int fn1() {
  foo(12);
}

int fn2() {
  fn1();
  bar(10);
}

int fn1bis() {
  foo(7);
}
