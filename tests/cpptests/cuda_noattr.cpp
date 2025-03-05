
__global__ void kernel_2(int i)
{
   unsigned int x = blockIdx.x*blockDim.x + threadIdx.x;
   unsigned int y = blockIdx.y*blockDim.y + threadIdx.y;
}

int main()
{
	return 0;
}
