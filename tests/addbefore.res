int main () {
  if (x) {
    foo();
    goto out;
  }
  after();
out:
  return 0;
}
