@@  @@
   // TODO: Marche pas
   void cpu_idle(...) {
   <...
- int idle = pm_idle;
    <...
-  idle();
+  nkidle();
    ...>
   ...>
   }
