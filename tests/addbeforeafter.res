int main () {
  if (x) {
    foo();
    goto out;
    bar();
  }
  after();
out:
  return 0;
}
