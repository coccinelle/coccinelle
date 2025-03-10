int main () {
  int rc;
  if (x) {
    if (y) {
      rc = 12;
      goto out;
    }
  }
  if (x) return 200;
out:
  return rc;
}
