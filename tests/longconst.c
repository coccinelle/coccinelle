long function1()
{
        long a;
        int b;
        a = 1l << b;
        a = 1u << b;
        a = 0x1fl << b;
        a = 0x1fu << b;
        a = 0x1FL << b;
        a = 0x1FU << b;
        return a;
}
