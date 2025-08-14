struct s { s() = delete; s(int){1;} };
int main(){ s v(1); }
