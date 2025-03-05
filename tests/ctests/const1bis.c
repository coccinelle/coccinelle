void foo(int j) { 
  const int i;
  int i;
  i++;
  const char *i;
  char *i;
  *i++;
}

void bar(int j) {
  int const i;
  int i;
  i++;
  char const *i;
  char *i;
  *i++;
}

void con_vol(int j) {
  const volatile int i;
}

void vol_con(int j) {
  volatile const int i;
}
