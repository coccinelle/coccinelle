int main () {
  foo(1);
  bar(2);
one:
  foo(3);
two:
  bar(4);
three:
  xxx(5);
#ifdef X
#endif
  foo(6);
#ifdef X
#endif
  bar(7);
#ifdef X
#endif
}
