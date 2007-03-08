//----------------------------------------------------------------------
// Rule 31
//----------------------------------------------------------------------

@@
identifier X;
@@

- #define DEVICE_INTR X

@@
statement S;
@@

  if(blk_queue_empty(QUEUE))
- {
-   CLEAR_INTR;
    S
- }

@@
@@

  if(blk_queue_empty(QUEUE)) {
-   CLEAR_INTR;
    ...
  }
