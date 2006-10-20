@@
identifier buffer, start, offset, length, inout, hostno;
identifier hostptr;
identifier proc_info_func;
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

@@ @@
  proc_info_func(...) {
    <...
-   hostno
+   hostptr->host_no
    ...>
  }

@@ 
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
