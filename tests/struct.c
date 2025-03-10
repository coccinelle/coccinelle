struct foo {
  int x;
  struct bar first;
  int y;
  struct xxx second;
  int z;
};

int main() {
  struct foo *a;
  f(a->first);
  f(a->second);
}

