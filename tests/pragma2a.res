#include <omp.h>
int main()
{
	int n = 3;
// notice the following does not match what's in the patch (n vs N)
#pragma omp target map(tofrom: u[0:n*n], u_tmp[0:n*N])
#pragma omp teams distribute parallel for simd
	for(int i=0;i<n;++i)
	{}
}
