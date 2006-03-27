@@
struct SHT sht; 
local function proc_info_func;
@@
   sht.proc_info = proc_info_func;

@@
identifier buffer, start, offset, length, inout, hostptr, hostno;
@@
- proc_info (char *buffer, char **start, off_t offset, int length, int hostno, int inout) {
+ proc_info (struct Scsi_Host *host, char *buffer, char **start, off_t offset, int length, int inout) {
    ...
-   struct Scsi_Host *hostptr;
    ...
-   hostptr = scsi_host_hn_get(hostno);
    ...
-  if (!hostptr) { ... }
    ...
-  scsi_host_put(hostptr);
    ...
  }

