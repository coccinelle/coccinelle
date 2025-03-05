aarg(1,2) int func() {
  int id aarg3(1,2);
  return 42;
}

aarg("not (1,2)") int func() {
  return 42;
}

int func() {
  return 42;
}

aarg2(arguments) int func() {
  return 42;
}

aarg2(1,"2") int func() {
  return 42;
}

void func() aarg4(1,2) {
}
