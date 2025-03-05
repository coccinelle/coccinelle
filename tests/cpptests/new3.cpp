#include <cassert>
#include <iostream>
struct A { int i{4}; A(){}; };
struct B { int i,j; };
struct C { int i;C(int j):i{j}{}; };
void n1(){
	// new new-type
	new A[3];
}
void n2(){
	// new new-type initializer
	B*b=new B    {1,2};
	assert(b->j==2);
}
void n3(){
	// new new-type initializer
	new C    (3);
}
void n4(){
	// new new-type initializer
	C*c=new C[2] {1,2};
	assert(c[0].i==1);
	assert(c[1].i==2);
}
void n5(){
	// new new-type
	new C*[2];
}
int main()
{
	n1(); n2(); n3();
	n4(); n5();
}
