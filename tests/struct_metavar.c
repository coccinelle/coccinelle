struct foo {
  int x;
  struct bar first;
  int y;
  struct xxx second;
  int z;
};

int main() {
  struct foo *a;
  struct notfoo *b;
  f(a->first);
  f(a->second);
  f(a->second);
  f(b->first);
  f(b->second);
}

