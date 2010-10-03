@@
struct gendisk g;
expression E;
@@

?- g.nr_real = E;

@@
struct gendisk g;
@@

error words = [g.nr_real]

@@
struct gendisk *A;
expression B, E;
struct gendisk *C;
int E1;@@

(
  add_gendisk(C+E1);
  ooo
- A[minor(B)].nr_sects = E;
+ set_capacity(C[&DEVICE_NR(B)],E);
|
- A->part[B].nr_sects = E;
+ set_capacity(A+B,E);
|
  add_gendisk(C+E1);
  ooo
- A[minor(B)].nr_sects
+ get_capacity(C[&DEVICE_NR(B)])
  ooo
|
  ...
- A->part[B].nr_sects
+ get_capacity(A+B)
  ...
)
