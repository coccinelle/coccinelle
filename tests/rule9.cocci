@@
identifier buffer, start, offset, length, inout, hostno;
identifier hostptr;
identifier proc_info_func;
//statement S;
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
-   if (!hostptr) { ... } // could be S instead of ...
    ...
-   scsi_host_put(hostptr);
    ...
  }

error words = [Scsi_Host]

