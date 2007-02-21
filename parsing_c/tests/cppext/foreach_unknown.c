void main(int i) {
  ULOOP (1 * 1000 * 1000) {
    STATUS;
    if (TIMEOUT)
      break;
  }
}
