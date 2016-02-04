int main() {
  if (foo())
    goto err;
  err: return;
}

