int main () {
  if (x) {
    a();
    b();
    c();
  }
  foo();
  while (x) {
    a();
    if (b()) continues;
    c();
  }
  foo();
  r();
  foo();
}

