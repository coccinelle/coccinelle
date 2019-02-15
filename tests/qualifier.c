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
};

int *baz(const struct bar *b)
{
    return (int *)&b->i;
}

int *baz2(struct bar *b)
{
    return (int *)&b->i;
}
