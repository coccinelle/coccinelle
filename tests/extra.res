#ifdef FIRST
int _called_function_0 (int a, struct foo *b, struct bar *c) {
  a = b->x;
  return c->d;
}

int main(int a, struct foo *b, struct bar *c) {
  assert(c != NULL);
  assert(b != NULL);
  return _called_function_0(a, b, c);
}
#else
int _called_function_1 (int a, struct foo *xyz) {
  a = xyz->x;
  return xyz->d;
}

int main(int a, struct foo *xyz) {
  assert(xyz != NULL);
  return _called_function_1(a, xyz);
}
#endif

