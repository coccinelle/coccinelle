int main (char c, int x) {}

int main (int *a, char c, int x) {}

int main (char c, int x, int *b) {}

int main (int *a, char c, int x, int *b) {}

int f() {
  f(12, e);
  f(12, e,1);
  f(0,12, e);
  f(0,12, e,1);
}
