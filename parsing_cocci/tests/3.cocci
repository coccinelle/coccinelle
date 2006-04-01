@@
struct Scsi_Host_Template sht;
!local function proc_info;
@@

sht.proc_info = proc_info;

@@
identifier buffer, start, offset, length, hostno, inout;
identifier host;
statement S1, S2;
expression E;
@@

- proc_info (char *buffer, char **start, off_t offset, int length, int hostno, int inout) {
+ proc_info (struct Scsi_Host *host, char *buffer, char **start, off_t offset, int length, int inout) {
    ...
-   struct Scsi_Host *host;
    ...
-   host = scsi_host_hn_get(hostno);
    ...
?-  if (host == NULL) S1 else S2
+   S2
    ...
?-  scsi_host_put(host);
    ...
  }

proc_info(...)
  {
    <...
(
-   E->host_no == hostno
+   E == shpnt
|
-   hostno
+   shpnt->host_no
)
    ...>
  }

