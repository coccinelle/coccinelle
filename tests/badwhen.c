int main () {
  f();
  if (foo()) return;
  g();
}
int second() {
  f();
  if (xfoo()) return;
  g();
}
