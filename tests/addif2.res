#ifdef FOO
int/*foo*/ xxx;
#endif
static int foo() {
  return 12;
}

#ifdef FOO
int/*foo*/ xxx;
#endif
static int bar() {
  return 12;
}
