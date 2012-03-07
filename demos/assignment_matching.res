int first() {
  int a = f(3);
  matches_little();
  if (c = f(3)) return 1;
  if (d = (int)f(3)) return 2;
  return 0;
}

int second() {
  int a = g(3);
  matches_more();
  if (matches_more()) return 1;
  if (d = (int)g(3)) return 2;
  return 0;
}

int third() {
  int a = h(3);
  matches_even_more();
  if (matches_even_more()) return 1;
  if (matches_even_more()) return 2;
  return 0;
}

int fourth() {
  int a = matches_most();
  b = matches_most();
  if (c = matches_most()) return 1;
  if (d = matches_most()) return 2;
  return 0;
}

