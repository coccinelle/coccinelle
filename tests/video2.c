int main(int x) {
  if (x == 1) {
    x();
    if (f()) return 12;
    if (g(1)) return 15;
    g(1);
    g(2);
    y();
    if (i()) return 120;
    g(2);
  }
  else if (x == 2) {
    x();
    if (f()) return 16;
    x();
    y();
    if (i()) return 160;
    g(2);
  }
  else if (x == 3) {
    x();
    if (f()) return 20;
    x();
    g(1);
    y();
    g(2);
  }
  else return 0;
}
