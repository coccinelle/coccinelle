#include <omp.h>
int main()
{
	int n = 3;
// FIXME: problem: uncommenting this breaks matching
#pragma omp target map(tofrom: u[0:n*n], u_tmp[0:n*n])
#pragma omp teams distribute parallel for simd
	for(int i=0;i<n;++i)
	{}
}
