@@
struct ctl_table table;
local function fn;
@@

table.proc_handler = fn;

@@
identifier A, B, C, D, E;
expression E1, E2, E3, E4, E5;
@@

- fn(ctl_table *A, int B, struct file *C, void *D, size_t *E) {
+ fn(ctl_table *A, int B, struct file *C, void *D, size_t *E, loff_t *ppos) {
    ooo
      <...
-     C->f_pos
+     *F
      ...>
    ooo
      <...
-     proc_dointvec(E1, E2, E3, E4, E5)
+     proc_dointvec(E1, E2, E3, E4, E5, *ppos)
      ...>
    ooo
      <...
-     proc_dostring(E1, E2, E3, E4, E5)
+     proc_dostring(E1, E2, E3, E4, E5, *ppos)
      ...>
    ooo
  }

error words = [proc_dointvec, proc_dostring, fn]
