void f(void) {
#define SEMICOLON() ;
  SEMICOLON()
#undef SEMICOLON
}

void g(void) {
#define SEMICOLON_HINT() MACROSTATEMENT
  SEMICOLON_HINT()
#undef SEMICOLON_HINT
}
