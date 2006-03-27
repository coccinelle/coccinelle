 struct { double j;int i;} i;

int f3() {
  i.i = 1;
}

int f1();
int f2();


void main () {
  f1();
  printf("%d\n", i.i);
}
