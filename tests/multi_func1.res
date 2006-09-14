int one() {
}

int two() {
}

int three() {
  bar(7);
}

int four() {
  foo(12);
}

int five() {
  bar(12);
}

int six() {
  one();
  two();
  three();
}

