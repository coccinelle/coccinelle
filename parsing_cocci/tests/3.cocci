@@
struct Scsi_Host_Template sht;
!local function proc_info_func;
@@

sht.proc_info = proc_info_func;

@@
identifier buffer, start, offset, length, inout, hostptr, hostno;
@@

  proc_info_func(
+      struct Scsi_Host *hostptr,
       char *buffer, char **start, off_t offset,
       int length, 
-      int hostno, 
       int inout) {
    ...
-   struct Scsi_Host *hostptr;
    ...
-   hostptr = scsi_host_hn_get(hostno);
    ...
?-  if (hostptr == NULL) { ... }
    ...
?-  scsi_host_put(hostptr);
    ...
  }

@@
expression E;
@@

proc_info_func(...) {
    <...
(
-   E->host_no == hostno
+   E == hostptr
|
-   hostno
+   hostptr->host_no
)
    ...>
  }

