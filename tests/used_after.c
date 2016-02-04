int my_proc_info(int i);

void f() {
  int x;
  x.proc_info = &my_proc_info;
}

int my_proc_info(int i) {
  return i++;
}
