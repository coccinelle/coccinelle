int main () {
  int *x;
  int *y;
  size_t a;
  ssize_t b;
  ptrdiff_t c;

  foo(ASIZE(sizeof ANINT(*x)),ASIZE(a));
  foo(ANINT(*x),ASSIZE(b));
  foo(APTRDIFF(x - y),APTRDIFF(c));
}
