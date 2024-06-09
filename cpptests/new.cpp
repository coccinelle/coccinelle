#include <new>
void n1(){
  int x;
  int*m{&x};
  int&r(x);

  // new (placement_params) (type) [initializer]
  new (m) (int) (r);
  new (m) (int) {r};
  new (m) (int);

  // new (placement_params) type [initializer]
  new (m) int (r);
  new (m) int {r};
  new (m) int;
}
void n2(){
  // new (type)
  new (int);
}
void n3(){
  // new (type) initializer
  new (int) (1);
}
void n4(){
  // new (type) initializer
  new (int) {1};
}
void n5(){
  // new type
  new int;
}
void n6(){
  // new type [initializer]
  new int (1);
}
void n7(){
  // new type [initializer]
  new int {1};
}

int main()
{
	n1();
	n2();
	n3();
	n4();
	n5();
	n6();
	n7();
}
