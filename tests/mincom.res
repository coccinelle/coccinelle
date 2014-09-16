int main () {
  if (rc == LS_NONE_FIRST_DE) {
     /* It is not "ls -{a}l" operation, no need statahead for it. */
     a();
     b();
  }
}
