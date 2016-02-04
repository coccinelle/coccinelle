@@
expression E;
identifier link;
@@

- void
+ int
axnet_config(struct pcmcia_device *link) {
  <...
(
  if (E) {
    ...
    axnet_release(...);
    ...
-   return;
+   return -ENODEV;
  }
|
  if (E) {
    ...
    cs_error(...);
    ...
-   return;
+   return -ENODEV;
  }
)
  ...>
}
