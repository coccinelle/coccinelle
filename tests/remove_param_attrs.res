int func1(int x, __nocast int y) {
	return 0;
}

int func2(int x, int __nocast y) {
	return 0;
}

int func3(int x, int y) {
	return 0;
}

int func1(int x, int y) {
	return 0;
}

int func2(int x, int y) {
	return 0;
}

int func3(int x, int y __attribute__((nocast))) {
	return 0;
}
