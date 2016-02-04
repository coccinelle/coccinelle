int main () {
  if (x) {
    goto out;
  }
  after();
out:
  return 0;
}
