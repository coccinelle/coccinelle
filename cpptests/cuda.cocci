# spatch --c++
@@
identifier kernel;
@@

+ // CUDA follows
  kernel<<< gridDim, blockDim, 0 >>>();
