struct foo {
  int a;
#define FOO 12
#define BAR 20
  int b;
#ifdef FOO
  int c;
#else
  int d;
#endif
};
