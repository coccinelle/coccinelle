long function1()
{
        long a;
        int b;
        a = f(1l, "long");
        a = f(1u, "unsigned");
        a = f(65536l, "long");
        a = f(65536u, "unsigned");
        a = f(65536, "int");
        a = f(4294967296, "int");
        a = f(65535, "int");
        a = f(4294967295, "int");
        a = f(0x7fffffff, "int");
        a = f(0x1fl, "long");
        a = f(0x1fu, "unsigned");
        a = f(0x1FL, "long");
        a = f(0x1FU, "unsigned");
        return a;
}
