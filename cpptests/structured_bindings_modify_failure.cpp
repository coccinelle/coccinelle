#include <initializer_list>
#include <tuple>
int main()
{
	int a [] = {0,1};
	auto & [b, c] = a;
}
