int main () {
  before();
  f(x);
  after();
  before();
  g(x);
  after();
}

int main1 () {
  before();
  f(x);
  if (x == NULL) {
    before();
    g(x);
  }
}

int main1 () {
  before();
  f(x);
  while (x == NULL) {
    if (q == 3) {
      before();
      g(x);
    }
  }
  x = 6;
}
int main2 () {
  before();
  f(x);
  if (x == NULL || y == 2) {
    before();
    g(x);
  }
}

