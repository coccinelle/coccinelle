int main () {
  if (x) { a(); } else { a(); }
}

int tochange () {
  if (x) { a(); } else { x(); }
}
