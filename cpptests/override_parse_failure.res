struct A { virtual int f(){ return 1; } };
struct B: public A { int f() override { return 1; } };
int main() { B b; }
