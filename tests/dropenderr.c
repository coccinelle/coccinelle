int main () {
  if (x) goto end;
  if (x) goto end2;
  return 0;
  end:
  end2:
  return -1;
}
