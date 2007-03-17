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
