int *foo(const int *i)
{
    return (int *)i;
}

int *foo2(int *i)
{
    return (int *)i;
}

struct bar {
    int i;
    int j[2];
};

int *baz(const struct bar *b)
{
    if (b->i)
        return (int *)&b->i;
    else
        return (int *)&b->j[0];
}

int *baz2(struct bar *b)
{
    if (b->i)
        return (int *)&b->i;
    else
        return (int *)&b->j[0];
}
