@disable optional_attributes@
identifier p;
type T;
@@
- T p __attribute__((myattr1));
+ T p __attribute__((section(".shared")));

@disable optional_attributes@
identifier p;
type T;
@@
- T p __attribute__((myattr2));
+ T p __attribute__((section(".shared")));

@disable optional_attributes@
identifier p;
type T;
@@
-T p __attribute__((myattr3));
+__attribute__((section(".shared")))
+T p;

@disable optional_attributes@
identifier p;
type T;
@@

-T p __attribute__((myattr4));
+T p;

@disable optional_attributes@
identifier p;
type T;
@@

-T p __attribute__((myattr4));
+T p;

@disable optional_attributes@
identifier p;
type T;
@@

-T p __attribute__((myattr5));
+T __attribute__((myattr5_infix)) p;
