typedef int *LPINT;

int foo(LPINT x, LPINT *y)
{
    return *x == **y;
}
