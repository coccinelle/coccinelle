#include <cassert>
void n1() {
  	// new type initializer
	int*a = new int(2);
	assert(a[0] == 2);
}
void n2() {
  	// new type
	int*b = new int[3];
	assert(b);
}
void n3() {
  	// new type initializer
	int*c = new int[2] {1,2};
	assert(c[0] == 1);
	assert(c[1] == 2);
}
int main()
{
	n1(); n2(); n3();
}
