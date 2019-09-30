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
    int j[2];
};

int *baz(const struct bar *b)
{
    if (b->i)
        return (survived_r1)&b->i;
    else
        return (survived_r1)&b->j[0];
}

int *baz2(struct bar *b)
{
    if (b->i)
        return &b->i;
    else
    return &b->j[0];
}
