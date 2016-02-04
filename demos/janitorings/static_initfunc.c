static int __init foo1(void)
{
}


int __init foo2(void)
{
}

static int foo3(void)
{
}


static int foo4(void)
{
}


module_init(foo1);
module_init(foo2);
