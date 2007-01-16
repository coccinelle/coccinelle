@@
identifier I;
identifier f;
@@

struct ctl_table I = {
  .proc_handler = f
};

@@
identifier ctl, write, filp, buffer, lenp;
// fresh identifier ppos;
expression A, B, C, D, E;
@@

  int cdrom_sysctl_info(struct ctl_table *ctl, int write, struct file * filp,
                           void *buffer, size_t *lenp
+                          , loff_t *ppos
  ) {
  <...
(
- proc_dointvec(A, B, C, D, E)
+ proc_dointvec(A, B, C, D, E, ppos)
|
- proc_dostring(A, B, C, D, E)
+ proc_dostring(A, B, C, D, E, ppos)
)
  ...>
  }

  