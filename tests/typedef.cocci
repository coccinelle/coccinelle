@ rule1 @
type T;
identifier link;
@@

T {
  ...
- dev_link_t link;
+ struct pcmcia_device *p_dev;
  ...
};

@ rule2 extends rule1 @
T *s;
identifier fld;
@@

- s->link.fld
+ s->p_dev->fld
