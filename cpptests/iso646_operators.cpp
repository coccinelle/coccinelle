int f(bool a, bool b, int x, int y)
{
	g();
	if (a and b)
		return x bitand y;
	if (a or not b)
		return x bitor y;
	if (a not_eq b)
		x and_eq y;
	x or_eq y;
	x xor_eq y;
	return compl x xor y;
}
