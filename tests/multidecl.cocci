@@
identifier driver;
identifier attach, detach;
@@

struct pcmcia_driver driver = {
	.remove		= detach
};

@@
identifier link;
@@

detach(struct pcmcia_device *link)
{
    ...
    if (link->state & DEV_CONFIG) {
	    ...
    }
    ...
}
