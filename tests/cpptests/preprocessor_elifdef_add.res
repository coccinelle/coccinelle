#undef Y
int main()
{
#if Y
	return 1;
#else
#if defined Y
	return 1;
#elifdef Y
	return 0;
#endif
#endif
}
