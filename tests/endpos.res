int main () {
  main();
  if (x) {
    foo();
    return -1;
  }
  if (x) {
    foo();
    goto out;
  }
  call();
  return 0;
out:
  print();
  return -1;
}

int main() {
  return 0;
}
