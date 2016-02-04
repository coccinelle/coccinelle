@@
filename A;
@@

--- a/.../A##.c
+++ b/.../A##.c
  ...
+ static struct request_queue A##_queue;

@@
local function fn;
identifier X;
@@

  fn (..., request_queue_t *X, ...) {
    <...
-   QUEUE
+   X
    ...>
  }

@@
@@

+ #define QUEUE(&X)
  <...
? QUEUE
  ...>

@@
@@

- BLK_DEFAULT_QUEUE(MAJOR_NR)
+ &A##_queue

@@
identifier i;
expression E, Y, Z;
@@

(
  for (i = 0; i < E; i++) {
    ...
+   Y->queue = &A##_queue;
    set_capacity(Y, Z);
    ...
  }
  ...
  for (i = 0; i < E; i++) {
    ...
    add_disk(Y);
    ...
  }
|
+   Y->queue = &A##_queue;
    set_capacity(Y, Z);
    ...
    add_disk(Y);
|
+   Y->queue = &A##_queue;
    add_disk(Y);
)
