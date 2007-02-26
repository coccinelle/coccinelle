@@
identifier f, p_dev, link;
@@

f(struct pcmcia_device *
-                       p_dev
+                       link
   ) {
  ...
- dev_link_t *link = dev_to_instance(p_dev);
  ...
}
