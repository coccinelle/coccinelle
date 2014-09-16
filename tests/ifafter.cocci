@@
identifier lbl1,lbl2;
expression e,e1,e2;
@@

+ e1=e2;
if (e)
-  GOTO(lbl1,e1=e2);
-GOTO(lbl2,e1=e2);
+  goto lbl1;
+goto lbl2;
