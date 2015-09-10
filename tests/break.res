int main () {
  while (1) {
    x = 12;
    do {
      x = 15;
      if (x > 1 ) { foo(); break; }
    } while (a == 3);
    if (x > 1 ) { foo();
      bar(); break; }
    if (x > 1 ) { foo();
      bar(); break; }
  }
}

int mainx () {
  while (1) {
    x = 12;
    do {
      x = 15;
      if (x > 1 ) { xxx(); continue; }
    } while (a == 3);
    if (x > 1 ) { xxx(); break; }
    if (x > 1 ) { xxx();
      bar(); continue; }
  }
}
