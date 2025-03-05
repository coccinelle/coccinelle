void foo() {
  int z[10];

  z[2] = 34;
}

int main() {
  int buf[20], foo[30];
  int i;

  for (i = 0; i <= 20; ++i) {
    buf[i] = i;
    foo[i] = i;
  }

  for (i = 0; i <= 20; ++i)
    printf("%d: %d\n", i, buf[i]);
}
