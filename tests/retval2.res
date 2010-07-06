int getlen(int *input, size_t maxlen, int delim, size_t *result) {
  size_t i;
  for (i = 0; i < maxlen; ++i) {
    if (input[i] == delim) {
      if (result != NULL) {
        *result = i;
      }
      return 0;
    }
  }
  return -1;
}

int newname(int *input, size_t maxlen, int delim, size_t *result) {
  size_t i;
  for (i = 0; i < maxlen; ++i) {
    if (input[i] == delim) {
      if (result != NULL) {
        *result = i;
      }
      return 0;
    }
  }
}

