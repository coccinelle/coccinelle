int var1_end __attribute__((section(".shared")));

int __attribute__((section(".shared"))) var1_mid;

int var2_end __attribute__((section(".shared")));

int __attribute__((section(".shared"))) var2_mid;

__attribute__((section(".shared")))
int var3_end;

__attribute__((section(".shared")))
int var3_mid;

int var4_end;

int var4_mid;

int __attribute__((section(".shared"))) var5_end;

int var5_mid __attribute__((section(".shared")));

char array1_end[2] __attribute__((section(".shared")));

char __attribute__((section(".shared"))) array1_mid[2];

char array2_end[2] __attribute__((section(".shared")));

char __attribute__((section(".shared"))) array2_mid[2];

__attribute__((section(".shared")))
char array3_end[2];

__attribute__((section(".shared")))
char array3_mid[2];

char array4_end[2];

char array4_mid[2];

char __attribute__((section(".shared"))) array5_end[2];

char array5_mid[2] __attribute__((section(".shared")));
