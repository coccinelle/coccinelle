#ifdef FOO
/* some comment */
int xxx() {
  /* a comment by itself */
  return 12;/* another comment */ }
#endif
static int foo() {
  return 12;
}

#ifdef FOO
/* some comment */
int xxx() {
  /* a comment by itself */
  return 12;/* another comment */ }
#endif
static int bar() {
  return 12;
}
