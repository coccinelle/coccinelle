#define METH(a) prefix_##a
void METH(foo)(int x){
 alloca(x);
}
