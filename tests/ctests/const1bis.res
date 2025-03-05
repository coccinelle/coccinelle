void foo(int j) { 
  float i;
  int i;
  i++;
  double *i;
  char *i;
  *i++;
}

void bar(int j) {
  float i;
  int i;
  i++;
  double *i;
  char *i;
  *i++;
}

void con_vol(int j) {
  const volatile long i;
}

void vol_con(int j) {
  volatile const long i;
}
