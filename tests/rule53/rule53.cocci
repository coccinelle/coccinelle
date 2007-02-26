@@
identifier I;
@@

Scsi_Host_Template I = {
-       .sg_tablesize           = ATA_MAX_PRD,
+       .sg_tablesize           = LIBATA_MAX_PRD,
};

@@
@@

- ATA_MAX_PRD / 2
+ LIBATA_MAX_PRD
