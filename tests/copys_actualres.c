/* not a res.c because obtaining this requires adding the argument
--macro-file-builtins soa.h */

int main () {
  {
    one();
    ALIGN(32)
      two();
    three();
  }
  if (x) {
    one();
    ALIGN(32) two();
    three();
  }
}
