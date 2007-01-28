@@
identifier driver;
identifier attach, detach;
@@

struct pcmcia_driver driver = {
	.probe		= attach,
	.remove		= detach
};

@@
//local function release;
identifier release;
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
identifier config;
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
-     config(link);
-     return 0;
+     return config(link);
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
fresh identifier probefn;
@@

- attach
+ probefn
       (struct pcmcia_device *link) { ... }

@@
@@

struct pcmcia_driver driver = {
	.probe		=
-                         attach
+                         probefn
};

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
