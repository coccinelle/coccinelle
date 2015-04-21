int p1() {
	int x = 0;
	int y = 1;

	x = x + y;
	x = x * y;
	x = x / y;
	x = x - y;
	x = x && y;
	x = x || y;
	x = x == y;
	x = x != y;
	x = x > y;
	x = x < y;
	x = x >= y;
	x = x <= y;
	x = x % y;
	x = x | y;
	x = x & y;
	x = x ^ y;
	x = x << y;
	x = x >> y;

	return 42;
}

int p2() {
	int x = 0;
	int y = 1;

	x = 1;
	x += 2;
	x -= 3;
	x *= 4;
	x /= 5;
	x %= 6;
	x |= 7;
	x &= 8;
	x ^= 9;
	x <<= 10;
	x >>= 11;

	return 42;
}
