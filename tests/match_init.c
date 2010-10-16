int probably_works () {
  int x = 3;
  f(x);
}

int does_it_work () {
  int y, x = 3;
  f(x);
}

int should_work () {
  x = 3;
  f(x);
}

