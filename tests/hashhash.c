#define FOO(x) foo ## x

int FOO(float)(int a, int b) {
  return 12;
}
