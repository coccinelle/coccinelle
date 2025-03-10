struct abcd {
	int a;
	int b;
	int c;
} __attribute__((pack,aligned(16)));

struct abcd {
	int a;
	int b;
	int c;
} __attribute__ ( ( pack , aligned(16) ) );

struct abcd {
	int a;
	int b;
	int c;
} __attribute__((aligned (16)));

struct abcd {
	int a;
	int b;
	int c;
};

struct abcd {
	int a;
	int b;
	int c;
} __attribute__((pack)) var1;

struct abcd {
	int a;
	int b;
	int c;
} __attribute__ ( ( pack ) ) var2;

struct abcd {
	int a;
	int b;
	int c;
} __attribute__((aligned (16))) var3;

struct abcd {
	int a;
	int b;
	int c;
} var4;
