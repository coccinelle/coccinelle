int main () {
  int *x;
  int *y;

  foo(ASIZE(sizeof(*x)));
  foo(ANINT(*x));
  foo(APTRDIFF(x - y));
}
