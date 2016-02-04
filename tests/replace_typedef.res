typedef struct foo { int x; } foo_t;

typedef int int_t;

int main() {
  struct foo x;
  int y;
  x.x = 12;
  return x.x + y;
}
