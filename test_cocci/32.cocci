@@
@@

--- a/drivers/cdrom/.../*.c
+++ b/drivers/cdrom/.../*.c

- #define CURRENT_VALID \
-         (!QUEUE_EMPTY && major(CURRENT -> rq_dev) == MAJOR_NR \
-          && CURRENT -> cmd == READ && CURRENT -> sector != -1)
+ static int current_valid(void) {
+   return !blk_queue_empty(QUEUE) &&
+          major(CURRENT->rq_dev) == MAJOR_NR &&
+          CURRENT->cmd == READ &&
+          CURRENT->sector != -1;
+ }
  <...
- CURRENT_VALID
+ current_valid()
  ...>
