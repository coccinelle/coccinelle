#include <omp.h>
int main()
{
	int n = 3;
// FIXME: problem: uncommenting this breaks matching
#pragma omp target map(tofrom: u[0:n*n], u_tmp[0:n*n])
#pragma omp loop
	for(int i=0;i<n;++i)
	{}
}
