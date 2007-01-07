//----------------------------------------------------------------------
// Rule 31
//----------------------------------------------------------------------

@@
text T;
@@

- #define DEVICE_INTR T

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
