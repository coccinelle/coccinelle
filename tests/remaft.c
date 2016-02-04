int main() {
#ifdef BLAH
  a();
#endif
  b();
  c()
#ifdef BLAH
  ;
#else
  + 4;
#endif
}
