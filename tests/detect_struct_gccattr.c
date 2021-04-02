struct abcd {
	int a;
	int b;
	int c;
} __attribute__((pack));

struct abcd {
	int a;
	int b;
	int c;
} __attribute__ ( ( pack ) );

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
