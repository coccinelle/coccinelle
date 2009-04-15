#define FOO_METH_TEST(a) prefix_##a
void FOO_METH_TEST(foo)(int x){
 malloc(x);
}
