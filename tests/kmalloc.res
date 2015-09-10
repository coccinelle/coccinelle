int main() {
  struct bar *y;
  struct foo *x = kzalloc(sizeof(struct foo), GPF_KERNEL);
  if (!x) return -ENOMEM;
  y = kzalloc(sizeof(struct bar), GPF_KERNEL);
  if (!y) return -ENOMEM;
}
