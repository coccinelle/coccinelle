int main() {
  int x;
  x = request_region(a,b,c);
  if (!x) { foo(); }
  if (x) { foo(); return 1; }
  if (x) { foo(); release_region(a,b); return 1; }
  release_region(a,b);
  if (!x) { foo(); }
  if (x) { foo(); return 1; }
  if (x) { foo(); release_region(a,b); return 1; }
}
