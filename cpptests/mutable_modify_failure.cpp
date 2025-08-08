struct S {
	mutable int x = 1;
	int y = 2;
};
int main()
{
	const S s;
	s.x = 0;
}
