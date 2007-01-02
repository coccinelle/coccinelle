//@@
//identifier driver;
//local function attach, detach;
//@@
//
//struct pcmcia_driver driver = {
//	...,
//	.probe		= attach,
//	.remove		= detach,
//	...
//};

@@
//local function detach, release;
identifier detach, release;
identifier link;
@@

detach(struct pcmcia_device *link)
{
    ...
    if (link->state & DEV_CONFIG) {
	    ...
	    release(link);
	    ...
    }
    ...
}

@@
//local function attach, config;
identifier attach, config;
identifier link;
@@

  attach(struct pcmcia_device *link)
  {
      ...
      config(link);
      ...
  }

@@
statement S;
@@

  attach(struct pcmcia_device *link)
  {
      ...
+     return
      config(link);
-     return 0;
  }

@@
// fresh identifier ret;
@@

  attach(struct pcmcia_device *link)
  {
+     int ret;
      ...
+     ret = 
      config(link);
+     if (ret) return ret;
      ...
  }

@@
@@

  attach(struct pcmcia_device *link)
  {
      <...
-     return -EINVAL;
+     return -ENOMEM;
      ...>
  }

@@
expression E;
identifier link;
@@

- void
+ int
config(struct pcmcia_device *link) {
  <...
(
  if (E) {
    ...
    release(...);
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

@@
@@

  config(...) {
    <...
-   return;
+   return 0;
    ...>
  }

@@
expression E;
statement S;
@@

  config(...) {
    ...
(
    return E;
|
    S
+   return 0;
)
  }
