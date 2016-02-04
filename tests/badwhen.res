int main () {
  f();
  if (foo()) return;
  g();
}
int second() {
  if (xfoo()) return;
}
