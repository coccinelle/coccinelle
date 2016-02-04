int nxnypz1(){
	bool z;
	int x, y;
	x = 1;
	y = 0;
	z = false;
	if (x)
		return x;

	return y;
}

int nxny2(){
	int x, y;
	x = 1;
	y = 4;

	if (x)
		return x;

	return y;
}

int nxny3() {
	int x;
	int y;

	x = (true)? 0 : 1;
	y = 4;

	return x;
}

int px4() {
	bool x;
	
	x = false;
	return 4;
}

int nxny5() {
	int x;
	int y;

	x = 0;
	y = 1;

	x = x + y;

	return 42;
}

int pxpy6() {
	bool x;
	
	bool y;
	

	x = false;
	y = true;

	x = x && y;

	return 42;
}

int nxny7() {
	int x;
	int y;

	x = 0;
	y = 1;

	x += y;

	return 42;
}
