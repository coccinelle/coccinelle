void
f()
{
    int k;
    bool b;
    bool *p;
    bool **x[];
    char *y[];
    f(1 == 2);
    f(1 & 3);
    g(1 & 2);
    g(1 != 2);
    g(1 <= 3);
    h(1 == 2);
    h(1 & 2);
    h(1 != 2);
    h(1 <= 2);
}

void
g(int a, int b, int c)
{
}
