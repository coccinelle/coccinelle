// the order of rules in standard.iso is important to makes this SP works.

@ rule1 @ 
typedef Scsi_Host_Template;
{struct SHT, Scsi_Host_Template} fops;
identifier proc_info_func;
@@
 fops.proc_info = proc_info_func;

@ rule2 @
identifier rule1.proc_info_func;
identifier buffer, start, offset, length, inout, hostno;
identifier hostptr;
typedef off_t;
@@
  proc_info_func (
+      struct Scsi_Host *hostptr,
       char *buffer, char **start, off_t offset, int length, 
-      int hostno, 
       int inout) {
    ...
-   struct Scsi_Host *hostptr;
    ...
-   hostptr = scsi_host_hn_get(hostno);
    ...
?-  if (!hostptr) { ... return ...; }
    ...
?-  scsi_host_put(hostptr);
    ...
  }

@ rule3 @ 
identifier rule1.proc_info_func;
identifier rule2.hostno;
identifier rule2.hostptr;
@@
  proc_info_func(...) {
    <...
-   hostno
+   hostptr->host_no
    ...>
  }

@ rule4 @ 
identifier rule1.proc_info_func;
identifier func; 
expression buffer, start, offset, length, inout, hostno;
identifier hostptr;
@@

 func(..., struct Scsi_Host *hostptr, ...) {
  <...
   proc_info_func(
+       hostptr,
        buffer, start, offset, length, 
-       hostno,
        inout)
   ...>
 }
