// this test concerns the lexing of the C file -- keywords like 'noinline' belonging to standard.h are problematic
int __attribute__((noinline)) foo(){}
int main(){foo();}
