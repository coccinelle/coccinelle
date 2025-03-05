int main()
{
	kernel_2<<<gridDim, blockDim, 0>>>();
}
