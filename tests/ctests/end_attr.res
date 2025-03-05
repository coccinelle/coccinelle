int main() {
  struct foo x;
  free(x);
  return;
}

int main() {
  struct foo x = NULL;
  free(x);
  return;
}

