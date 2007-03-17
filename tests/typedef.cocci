@@
type T;
identifier link;
@@

T {
  ...
- dev_link_t link;
+ struct pcmcia_device *p_dev;
  ...
}

@@
T *s;
identifier fld;
@@

- s->link.fld
+ s->p_dev->fld

@@
@@

-	dev_link_t *link;
+	dev_link_t *link = dev_to_instance(p_dev);
	...
-	link = &info->link;
+	info->p_dev = p_dev;
