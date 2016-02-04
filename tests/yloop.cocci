@@
identifier buffer;
identifier hostptr;
@@
  arxescsi_proc_info (
+      struct Scsi_Host *hostptr,
       char *buffer) {
    ...
-   hostptr = scsi_host_hn_get(hostno);
    ...
  }
