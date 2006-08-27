int my_proc_info(int i);

void f1() {
  int x;
}

void f2() {
  int x;
  x.proc_info = &my_proc_info;
}

int my_proc_info(int i) {
  return i++;
}


int not_a_proc_info_function(int i) {
  return i++;
}
