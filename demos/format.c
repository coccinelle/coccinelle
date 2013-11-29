int main () {
  foo("one %d two %2x three\n");
  foo("blah %d two %2x three %s xxx %d");
  foo("xyz %d %d %0.2f %s three\n");
  foo("xyz %d %0.2f abc");
  foo("xxx %s");
}
