// for seeing when errorexit nodes are created

int main () {
  int bad = -1;
  if (x < 100) return -ENOMEM;
  if (x < 100) return bad;
  if (x < 100) goto out;
  return 0;
out:
  return bad;
}
