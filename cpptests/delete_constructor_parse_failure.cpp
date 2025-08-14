struct s { s() = delete; s(int){0;} };
int main(){ s v(0); }
