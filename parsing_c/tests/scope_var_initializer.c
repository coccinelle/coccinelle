struct foo { int a; };

int main() {
  struct foo *x = f(sizeof(*x));
  return x->a;
}
