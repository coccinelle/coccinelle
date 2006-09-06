int main() {
  int x;
  x = request_region(a,b,c);
  if (!x) { foo(); }
  if (x) { foo(); return 1; }
  if (x) { foo(); release_region(a,b); return 2; }
  if (x) { foo(); release_region(a,b); return 3; }
  release_region(a,b);
  if (!y) { foo(); }
  if (y) { foo(); return 1; }
  if (y) { foo(); release_region(a,b); return 1; }
}
