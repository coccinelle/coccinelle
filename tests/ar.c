struct bar { struct foo *a; struct foo b[27]; };

int main() {
  struct foo *x;
  struct bar *y;
  struct foo z[15];

  x->y = 12;
  y->a->y = 12;
  y->b[7].y = 12;
  z[15].y = 12;
}
