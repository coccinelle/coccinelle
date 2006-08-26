int my_proc_info(int i);

void f() {
  int x;
}

int my_proc_info(int i) {
  return i++;
}


int not_a_proc_info_function(int i) {
  return i++;
}
