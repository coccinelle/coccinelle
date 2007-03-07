struct foo {
  ...
  struct bar first;
  ...
  struct xxx second;
  ...
};

int main() {
  struct foo *a;
  f(a->first);
  f(a->second);
}

