@func@
typedef int64_t;
typedef uint64_t;
identifier f;
position p;
expression E;
type T;
{int,unsigned,long,unsigned long} i;
@@

(
int64_t
|
uint64_t
) f(...) {
(
int64_t
|
uint64_t
) a;
  ...
+ xxx();
  return <+... i <<@p E ...+>;
  ...
}
