#define FOO 10

void foo()
{
  int i;

  if (!i &  FOO) return;
  !i & -FOO;
  !i & !FOO;
  !i &  100;
  !i & -100;
  !i & !100;
}
