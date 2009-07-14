int main () {
  if (retval) {
    if (retval != -ENODEV) {
      foo();
      return 3;
    }
    bar();
  }
  return 6;
}

int second () {
  if (retval) {
    if (retval != -ENODEV) {
      foo();
      goto out;
    }
    bar();
  }
out:
  return 6;
}


