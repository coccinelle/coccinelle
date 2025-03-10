void foo(int y) {
  int x;
  if (x) { aaa(); bbb(); before_return();
    return; }
  if (x) { aaa(); bbb(); before_return();
    return; }
  ccc();
  before_return();
}
