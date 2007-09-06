int bar(void)
{
	return 12;
}


int foo()
{
	bar();

}

void main(void)
{
	printf("%d\n", foo());
}
