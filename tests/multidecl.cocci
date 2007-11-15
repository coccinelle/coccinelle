// test with -cocci_vs_c_3 -use_ref

@ rule1 @
identifier driver;
identifier attach, detach;
@@

struct pcmcia_driver driver = {
	.remove		= detach,
};

@@
identifier link;
identifier rule1.detach;
@@

detach(struct pcmcia_device *link)
{
    ...
    if (link->state & DEV_CONFIG) {
	    ...
    }
    ...
}
