@@
identifier hostptr2, hostno2;
@@
  hostptr2 = scsi_host_hn_get(hostno2);

  scsi_host_put(hostptr2);

  struct Scsi_Host *hostptr2;

  if (!hostptr2) { ... }
