#include <initializer_list>
#include <tuple>
int main()
{
	int d [] = {0,1};
	auto & [b, c] = d;
}
