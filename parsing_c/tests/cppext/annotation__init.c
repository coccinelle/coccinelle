/* gcc -E init.c */

//#define MODULE // 
#include <linux/init.h>

static void __init initme(int x, int y)
{
    int z; z = x * y;
}

static void __exit exitme(int x, int y)
{
    int z = x * y;
}

