int main () {
  if (retval) {
      foo();
      return 3;
  }
  return 6;
}

int second () {
  if (retval) {
      foo();
      goto out;
  }
out:
  return 6;
}


