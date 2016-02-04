int main () {
#ifdef FOO
  free(foo);
#else
  x = foo->x;
#endif
}
