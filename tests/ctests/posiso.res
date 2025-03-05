int main () {
  int *x;
  if (!x) x = a; else x = b;
  if (x == a) x = a; else x = b;
  if (!x) x = a;
  if (x == a) x = a;
}


