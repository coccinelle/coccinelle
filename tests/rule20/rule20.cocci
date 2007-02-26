@@
//constant YYY;
expression YYY;
identifier dev_info;
@@

static dev_info_t dev_info = YYY;

@@
//local function init;
identifier init;
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
  int init (...) {
    servinfo_t serv;
    ...
-   CardServices(GetCardServicesInfo, &serv);
    ...
-   if (serv.Revision != CS_RELEASE_CODE) { ... }
    ...
    register_pccard_driver(&dev_info, &XXX_attach, &XXX_detach)
    ...
  }

@@
@@

  init (...) {
-   servinfo_t serv;
    ... when != serv
  }

@@
identifier err;
@@

  init (...) {
-   int err;
    ... when != err
-   err = register_pccard_driver(&dev_info, &XXX_attach, &XXX_detach);
-   return err;
+   return pcmcia_register_driver(&ZZZ_driver);
  }

@@
@@

  init (...) {
    ...
-   register_pccard_driver(&dev_info, &XXX_attach, &XXX_detach);
-   return 0;
+   return pcmcia_register_driver(&ZZZ_driver);
  }

@@
// fresh identifier err;
@@

  init (...) {
+   int error;
    ...
-   register_pccard_driver(&dev_info, &XXX_attach, &XXX_detach);
+   error = pcmcia_register_driver(&ZZZ_driver);
+   if (error) return error;
    ...
    return 0;
  }


@@
@@

  init (...) {
    <...
-   register_pccard_driver(&dev_info, &XXX_attach, &XXX_detach)
+    pcmcia_register_driver(&ZZZ_driver);
    ...>
  }


@@
//local function exit;
identifier exit;
@@

//__exit
  exit (...) {
    ...
-   unregister_pccard_driver(&dev_info);
+   pcmcia_unregister_driver(&ZZZ_driver);
    ...
  }

// doesn't seem exactly related to the CE, but many files have this change
// as well

// a bit overspecific that the call to DEBUG has to be the first instruction,
// but in some cases it comes under an if and we don't want to delete it
// no way to specify that...

@@
@@
  init(...) {
-   DEBUG(...);
    ...
  }

@@
@@
  exit(...) {
-   DEBUG(...);
    ...
  }
