void main() {
  if (option & 15)
    printk(KERN_INFO "%s: ignoring user supplied media type %d",
           dev->name, option & 15);
}
