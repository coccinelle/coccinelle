int main() {
  struct foo x autofree;
  return;
}

int main() {
  struct foo x autofree = NULL;
  return;
}

