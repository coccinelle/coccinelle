int main(int x) {
  foo();
  if (x) {bar(); after(); return 0;}
  bar();
  after();
}

