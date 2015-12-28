int main (int x) {}

int main (int *a, int x) {}

int main (int x, int *b) {}

int main (int *a, int x, int *b) {}

int f() {
  f(e);
  f(e,1);
  f(0,e);
  f(0,e,1);
}
