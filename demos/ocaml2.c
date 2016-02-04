int main () {
  if (f(3)) goto l;
  if (f(x)) x = 2;
  if (f(x+y)) return;
  l: return;
}

