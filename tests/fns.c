int __init probe_base_port(int base) {
  for (base = b; base <= e; base += 0x10) {
    if (check_region(base, 0x10))
      continue;
    return (base);
  }
  return 0;
}

int __init cm206_init(void) {
  request_region(cm206_base, 0x10, "cm206");
}
