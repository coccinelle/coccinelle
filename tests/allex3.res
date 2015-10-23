int main () {
  int rc;
  if (x) {
    if (y) {
      rc = 12;
      goto out;
    }
    goto out;
  }
  return 15;
out:
}
