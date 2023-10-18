
__global__ void kernel_1()
{
   unsigned int x = blockIdx.x*blockDim.x + threadIdx.x;
   unsigned int y = blockIdx.y*blockDim.y + threadIdx.y;
}

__global__ void kernel_2(int i)
{
   unsigned int x = blockIdx.x*blockDim.x + threadIdx.x;
   unsigned int y = blockIdx.y*blockDim.y + threadIdx.y;
}

int main()
{
	dim3 blockDim(16, 16, 1);
	dim3 gridDim(8, 8, 1);
	kernel_2< < < gridDim, blockDim, 0 > > >(); // bad
	kernel_1< gridDim, blockDim, 0 >();
	f();
	return 0;
}
