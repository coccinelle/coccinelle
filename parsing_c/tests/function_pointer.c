

static int (*pointer_func)(int);

int main(int i) { 
  int (*f)(int i);

  f(0);
}
