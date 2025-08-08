struct A { virtual int f(){ return 2; } };
struct B: public A { int f() override { return 0; } };
int main() { B b; }
