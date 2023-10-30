// From Tim Mattson's https://www.youtube.com/watch?v=Rde6kpv16-4  at 6'20"
@@
identifier i,n;
@@
// FIXME: can't uncomment yet following, because uncommenting it in C file is broken
  #pragma omp target map(tofrom: u[0:n*n], u_tmp[0:n*n])
- #pragma omp teams distribute parallel for simd
+ #pragma omp loop
  for(int i=0;i<n;++i) { ... }
