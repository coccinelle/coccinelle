//----------------------------------------------------------------------
// Rule 31
//----------------------------------------------------------------------

@@
identifier X;
@@

- #define DEVICE_INTR X


// me: why not simply - CLEAR_INTR; ? why all those cases ?
// to also remove sometimes the { } ?

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

@@
@@
  if(blk_queue_empty(QUEUE)) {
   ... when = \( printk(...); \| dbg(...); \)
-   CLEAR_INTR;
    ...
  }


