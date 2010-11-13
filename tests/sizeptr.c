int main () {
  int *x;
  int *y;
  size_t a;
  ssize_t b;
  ptrdiff_t c;

  foo(sizeof *x,a);
  foo(*x,b);
  foo(x-y,c);
}
