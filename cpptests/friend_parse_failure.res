struct s { void f(){} };
struct z { friend struct s; void f(){1;} };
int main(){}
