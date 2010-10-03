int main () {
  f(x);
  g(x);
}

int main1 () {
  f(x);
  if (x == NULL) {
    g(x);
  }
}

int main1 () {
  f(x);
  while (x == NULL) {
    if (q == 3) {
      g(x);
    }
  }
  x = 6;
}
int main2 () {
  f(x);
  if (x == NULL || y == 2) {
    g(x);
  }
}

