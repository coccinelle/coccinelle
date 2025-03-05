@@
expression E;
identifier config;
@@

config(struct pcmcia_device *link) {
    ...
+   return
    bar();
-   return 0;
}
