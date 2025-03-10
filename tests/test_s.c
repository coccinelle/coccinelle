int main () {
  if (x-one) {
    one();
  }
  if (x-two) {
    one();
    two();
  }
  if (x-three) {
    one();
    two();
    three();
  }
  if (two) {
    while (x) {
      one();
    }
    while (x) {
      one();
      two();
    }
  }
  if (one) {
    while (x) {
      one();
    }
  }
  if (three) {
    while (x) {
      one();
    }
    while (x) {
      one();
      two();
    }
    while (x) {
      one();
      two();
    }
  }
}
