main (int x);
static xmain (int x);
inline ymain (int x);

main (int x) {
  return x;
}
// foo

static xmain (int y) {
  return y;
}
// xxx

inline ymain (int y) {
  return y;
}
// xxx
