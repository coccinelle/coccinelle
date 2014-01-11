int main() {
#ifdef BLAH
#endif
  c()
#ifdef BLAH
  ;
#else
  + 4;
#endif
}
