@ rule1 @
type T;
identifier link;
@@

T {
  ...
- dev_link_t link;
+ struct pcmcia_device *p_dev;
  ...
}

@ rule2 extends rule1 @
//T *s;
identifier fld;
identifier fn;
identifier s;
@@

fn(...,T *s,...) {
 <...
- s->link.fld
+ s->p_dev->fld
 ...>
}
