@@
//constant YYY;
expression YYY;
identifier dev_info;
@@

static dev_info_t dev_info = YYY;

@@
local function init;
identifier serv;
identifier XXX_attach, XXX_detach;
fresh identifier ZZZ_driver;
@@

+ static struct pcmcia_driver ZZZ_driver = {
+       .owner          = THIS_MODULE,
+       .drv            = {
+               .name   = YYY,
+       },
+       .attach         = XXX_attach,
+       .detach         = XXX_detach,
+ };

//__init
  int initfn (...) {
    servinfo_t serv;
    ...
-   CardServices(GetCardServicesInfo, &serv);
    ...
-   if (serv.Revision != CS_RELEASE_CODE) { ... }
    ...
    register_pccard_driver(&dev_info, &XXX_attach, &XXX_detach);
    ...
    return 0;
  }

@@
@@

  initfn (...) {
    servinfo_t serv;
    ... when != serv
  }

@@
@@

  initfn (...) {
    ...
-   register_pccard_driver(&dev_info, &XXX_attach, &XXX_detach);
-   return 0;
+   return pcmcia_register_driver(&ZZZ_driver);
  }

@@
// fresh identifier err;
@@

  initfn (...) {
+   int err;
    ...
-   register_pccard_driver(&dev_info, &XXX_attach, &XXX_detach);
+   err = pcmcia_register_driver(&ZZZ_driver);
+   if (err) { return err; }
    ...
-   return 0;
  }

@@
local function exit;
@@

//__exit
  exitfn (...) {
    ...
-   unregister_pccard_driver(&dev_info);
+   pcmcia_unregister_driver(&ZZZ_driver);
    ...
  }
