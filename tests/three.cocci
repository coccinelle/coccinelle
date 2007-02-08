@@
identifier init;
identifier XXX_attach, XXX_detach; // pad: XXX_detach useful ?
@@

  int init (...) {
    ...
    register_pccard_driver(&XXX_attach)
    ...
  }

@@
@@

  init (...) {
    ...
-   register_pccard_driver(&XXX_attach);
-   return 0;
+   return pcmcia_register_driver(&ZZZ_driver);
  }

@@
identifier exit;
@@

  exit (...) {
    ...
-   unregister_pccard_driver(&dev_info);
+   pcmcia_unregister_driver(&XXX_attach);
    ...
  }
