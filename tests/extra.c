#ifdef FIRST
int main (int a, struct foo *b, struct bar *c) {
  a = b->x;
  return c->d;
}
#else
int main (int a, struct foo *xyz) {
  a = xyz->x;
  return xyz->d;
}
#endif

