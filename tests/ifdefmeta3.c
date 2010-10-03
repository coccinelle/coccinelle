int main () {
  f();
  if (foo) {
    one();
#ifdef ONE
    two();
#endif
    three();
  }
  g();
}
