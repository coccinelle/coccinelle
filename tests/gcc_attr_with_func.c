int __attribute__((always_inline)) func (int a);

int func(int a);

int __attribute__((always_inline)) func (int a) {
  return 42;
}

int func (int a) {
  return 42;
}

int __attribute__((always_inline)) *func (int a) {
  return 42;
}

int func (int a) __attribute__((attr)) {
  return 42;
}

void __attribute__((mult,"1",2)) func (int a) {
}

void __attribute__((mult,1,"2")) func (int a) {
}
