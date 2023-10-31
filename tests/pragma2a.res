#include <omp.h>
int main()
{
	int n = 3;
// FIXME: problem: notice the following does not match what's in the patch
#pragma omp target ahiaahiaahiaahiaahiaahia
#pragma omp teams distribute parallel for simd
	for(int i=0;i<n;++i)
	{}
}
