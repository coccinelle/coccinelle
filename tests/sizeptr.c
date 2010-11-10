int main () {
  int *x;
  int *y;

  foo(sizeof(*x));
  foo(*x);
  foo(x-y);
}
