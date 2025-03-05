int main() {
  struct bar *y;
  struct foo *x = kmalloc(sizeof(struct foo),GPF_KERNEL);
  if (!x) return -ENOMEM;
  y = kmalloc(sizeof(struct bar),GPF_KERNEL);
  if (!y) return -ENOMEM;
  memset(x,0,sizeof(struct foo));
  memset(y,0,sizeof(struct bar));
}
