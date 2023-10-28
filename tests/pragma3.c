#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wgnu-zero-variadic-macro-arguments" /* for gtest under clang (yes, with '#pragma GCC') */

#pragma GCC top
int main () {
  #pragma GCC inside
  return 0;
}
