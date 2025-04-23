void foo(int x, int y, int z) {
  scoped_guard(wiphy) {
    tmp = get_wiphy_regdom(wiphy);
    x = y + z;
    if (x) { aaa(); before_return();
      return; }
    rcu_free_regdom(tmp);
  }
  before_return();
  return;
}
