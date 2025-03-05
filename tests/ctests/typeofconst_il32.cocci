#spatch --int-bits 32 --long-bits 32
@@
int si;
long sl;
long long sll;
unsigned int ui;
unsigned long ul;
unsigned long long ull;
@@

(
- si
+ INT(si)
|
- sl
+ LONG(sl)
|
- sll
+ LONGLONG(sll)
|
- ui
+ UINT(ui)
|
- ul
+ ULONG(ul)
|
- ull
+ ULONGLONG(ull)
)
