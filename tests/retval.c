int main () {
  if (retval1) {
    if (retval2 != -ENODEV) {
      foo();
      return 3;
    }
    bar();
  }
  return 6;
}

int second () {
  if (retval1) {
    if (retval2 != -ENODEV) {
      foo();
      goto out;
    }
    bar();
  }
out:
  return 6;
}


