int main () {
  if (x) { a(); } else { a(); }
}

int has_no_a () {
  if (x) { a(); } else { x(); }
}
