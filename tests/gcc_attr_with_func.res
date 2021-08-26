int __attribute__((always_inline)) func ();

int func(long a);

int __attribute__((always_inline)) func () {
  return 42;
}

int func (int a) {
  return 42;
}

int __attribute__((always_inline)) *func (long a) {
  return 42;
}

int func (long a) __attribute__((attr)) {
  return 42;
}
