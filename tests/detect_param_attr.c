int func1(int __nocast u, __nocast int z, int q __nocast, int w) {
	return 0;
}

int func1(int __attribute__((nocast)) u, __attribute__((nocast)) int z, int q __attribute__((nocast)), int w) {
	return 0;
}
