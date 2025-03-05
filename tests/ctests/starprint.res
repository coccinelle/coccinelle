typedef int *LPINT;

int foo(int *x, int **y)
{
    return *x == **y;
}
