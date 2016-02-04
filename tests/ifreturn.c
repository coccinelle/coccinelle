int main () {
  if (x) {
    return 1;
  }
  return 12;
}

int main () {
  if (a) {
    if (x) {
      return 1;
    }
    b();
  }
  return 12;
}

int main () {
  if (a) {
    if (b) {
      if (x) {
        return 1;
      }
      else {
        return 1;
      }
    }
    b();
  }
  return 12;
}
