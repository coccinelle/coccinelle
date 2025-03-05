int func1(int x, __nocast int y) {
	return 0;
}

int func2(int x, int __nocast y) {
	return 0;
}

int func3(int x, int y __nocast) {
	return 0;
}

int func1(int x, __attribute__((nocast)) int y) {
	return 0;
}

int func2(int x, int __attribute__((nocast)) y) {
	return 0;
}

int func3(int x, int y __attribute__((nocast))) {
	return 0;
}
