int __attribute__((always_inline)) func ();

int func(int a);

int __attribute__((always_inline)) func () {
  return 42;
}

int func (int a) {
  return 42;
}
