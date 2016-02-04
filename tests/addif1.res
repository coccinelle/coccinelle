#ifdef FOO
int xxx() {
  return 12; }
#endif
static int foo() {
  return 12;
}

#ifdef FOO
int xxx() {
  return 12; }
#endif
static int bar() {
  return 12;
}
