#define METH(a) prefix_##a
void METH(foo)(int x){
 malloc(x);
}
