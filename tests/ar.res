struct bar { struct foo *a; struct foo b[27]; };

int main() {
  struct foo *x;
  struct bar *y;
  struct foo z[15];
}
