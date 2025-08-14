struct s { void f(){} };
struct z { friend struct s; void f(){0;} };
int main(){}
