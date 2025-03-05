long function1()
{
        long a;
        int b;
        a = 1l << b;
        a = 1u << b;
        a = 65536l << b;
        a = 65536u << b;
        a = 65536 << b;
        a = 4294967296 << b;
        a = 65535 << b;
        a = 4294967295 << b;
        a = 0x7fffffff << b;
        a = 0x1fl << b;
        a = 0x1fu << b;
        a = 0x1FL << b;
        a = 0x1FU << b;
        return a;
}
