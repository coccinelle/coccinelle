int var1 __attribute__((section(".shared")));

int var2 __attribute__((section(".shared")));

__attribute__((section(".shared")))
int var3;

int var4;

int var5__attribute__((myattr5_infix));

char array1[2] __attribute__((section(".shared")));

char array2[2] __attribute__((section(".shared")));

__attribute__((section(".shared")))
char array3[2];

char array4[2];

char __attribute__((myattr5_infix)) array5[2];

long *pointer1 __attribute__((section(".shared")));

long *pointer2 __attribute__((section(".shared")));

__attribute__((section(".shared")))
long *pointer3;

long *pointer4;

long *__attribute__((myattr5_infix)) pointer5;
