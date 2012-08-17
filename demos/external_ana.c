/* An indirect way to compute zero, for an external analysis to find out. */

#include <stdio.h>

int sum(const int x, const int y) {
  return x + y;
}

int add4(const int z) {
  return sum(z, 1) + 3;
}

int main() {
  int x = add4(1) - 5;   /* should be zero */

  if (x) {
    printf("Dead code.");
  }

  int y = getchar() < 0; /* can happen */
  if (y) {
    printf("Not dead code.");
  }

  return 0;
}
