@@
struct gendisk g;
expression E;
@@

- g.nr_real = E;

@@
struct gendisk g;
@@

error words = [g.nr_real]

@@
struct gendisk *A;
expression B, E;
@@

(
  add_gendisk(C+E1);
  ooo
- A[minor(B)].nr_structs = E;
+ set_capacity(C[&DEVICE_NR(B)],E);
|
- A->part[B].nr_structs = E;
+ set_capacity(A,E);
|
  add_gendisk(C+E1);
  ooo
- A[minor(B)].nr_structs;
+ get_capacity(C[&DEVICE_NR(B)]);
|
- A->part[B].nr_structs
+ get_capacity(A)
)
