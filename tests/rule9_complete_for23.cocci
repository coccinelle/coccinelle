@@
identifier buffer, start, offset, length, inout, hostno;
identifier hostptr;
identifier proc_info_func;
expression E;
@@
  proc_info_func (
+      struct Scsi_Host *hostptr,
       char *buffer, char **start, off_t offset, int length, 
-      int hostno, 
       int inout) {
    ...
-   struct Scsi_Host *hostptr;
    ...
-   if((hostptr = scsi_host_hn_get(hostno))==NULL) return ...;
    ...
?-  scsi_host_put(hostptr);
    ...
  }

error words = [scsi_host_hn_get,scsi_host_put]

@@
@@
  proc_info_func(...) {
    <...
-   hostno
+   hostptr->host_no
    ...>
  }
