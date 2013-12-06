int main () {
  if (x) { xyz1(); }
  if (x) { foo(); abc(); }
  if (x) { xyz2(); }
  if (x) { bar(); abc(); }
  if (x) { xyz3(); }
  if (x) { foo(); abc(); bar(); bar(); foo(); }
}
