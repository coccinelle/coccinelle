@@
function xxx_info;
identifier x,y;
@@
  int xxx_info(int x
+              ,scsi *y 
                     ) {
   ...
-  scsi *y;
   ...
-  y = scsi_get();
-  if(!y) { ... return -1; }
   ...
-  scsi_put(y);
   ...
}


