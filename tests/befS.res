int main () {
  foo();
  xxx();
  foo();
  if(y) {
    foo();
    {
    foo();
    rrr();
  }
}
}


int d() {}

int main2 () {
  foo();
  yyy();
  foo();
  xxx();
}
