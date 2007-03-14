//@@ 
//typedef Scsi_Host_Template;
//{struct SHT, Scsi_Host_Template} fops;
//identifier proc_info_func;
//@@
// fops.proc_info = proc_info_func;

//@@
//identifier template_struct, proc_info_func;
//typedef Scsi_Host_Template;
//@@
//
////struct SHT template_struct = {
//Scsi_Host_Template template_struct = { 
// ... 
// .proc_info = proc_info_func, 
// ...
//};

@@
identifier proc_info_func; // comment if use above code
identifier buffer, start, offset, length, inout, hostno;
identifier hostptr;
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
?-  if (!hostptr) { ... return ...; }
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
