int f1() {
  foo(12);
}

int f2() {
  bar(12);
}

int f3() {
  bar(7);
}

int f4() {
  foo(12);
}

int f5() {
  bar(12);
}

int main() {
  f1();
  f2();
  f3();
}

