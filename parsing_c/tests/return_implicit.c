int bar(void)
{
	return 12;
}


int foo()
{
	bar();

}

int fact(int n)
{
	if(n == 0) {
		bar();
	} else {
		bar();
	}
}       
		

void main(void)
{
	printf("%d\n", foo());
	printf("%d\n", fact(3));
}
