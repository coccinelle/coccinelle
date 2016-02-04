int main () {
  if (retval1) {
    foo();
    return 3;
  }
  return 6;
}

int second () {
  if (retval1) {
    foo();
    goto out;
  }
out:
  return 6;
}


