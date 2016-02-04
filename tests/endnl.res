int main () {
#ifdef FOO
  call(1);
#endif
  foo(1);
  bar(2);
#ifdef BAR
  call(2);
#endif
one:
#ifdef FOO
  call(3);
#endif
  foo(3);
two:
  bar(4);
#ifdef BAR
  call(4);
#endif
three:
  xxx(5);
#ifdef X
#endif
#ifdef FOO
  call(6);
#endif
  foo(6);
#ifdef X
#endif
  bar(7);
#ifdef BAR
  call(7);
#endif
#ifdef X
#endif
}
