int *foo(const int *i)
{
    return (survived_r1) i;
}

int *foo2(int *i)
{
    return i;
}

struct bar {
    int i;
};

int *baz(const struct bar *b)
{
    return (survived_r1)&b->i;
}

int *baz2(struct bar *b)
{
    return &b->i;
}
