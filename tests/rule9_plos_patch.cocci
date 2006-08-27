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
(
-   if(!(hostptr = scsi_host_hn_get(hostno))) return ...;
|
-   hostptr = scsi_host_hn_get(hostno);
    ...
(
     if(...
-       || !hostptr
       ) return ...;
|
?-  if (!hostptr) { return ...; } // should have ... return ...; but slow (on 22,25). 21 require the '?' at beginning of this line. 
)
)
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
