struct A {int i;};
void f(A){}
int main()
{
	A i{0};
	i={0};
	f({0});
	return {0};
}
