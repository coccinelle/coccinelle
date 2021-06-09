@disable optional_attributes@
identifier p;
type T;
@@
- T p __attribute__((myattr1_end));
+ T p __attribute__((section(".shared")));

@disable optional_attributes@
identifier p;
type T;
@@
T p
-__attribute__((myattr2_end));
+__attribute__((section(".shared")));

@disable optional_attributes@
identifier p;
type T;
@@
-T p __attribute__((myattr3_end));
+__attribute__((section(".shared")))
+T p;

@disable optional_attributes@
identifier p;
type T;
@@

-T p __attribute__((myattr4_end));
+T p;

@disable optional_attributes@
identifier p;
type T;
@@

-T p __attribute__((myattr5_end));
+T __attribute__((section(".shared"))) p;

@disable optional_attributes@
identifier p;
type T;
@@
- T __attribute__((myattr1_mid)) p;
+ T __attribute__((section(".shared"))) p;

@disable optional_attributes@
identifier p;
type T;
@@
T
-__attribute__((myattr2_mid))
+__attribute__((section(".shared")))
p;

@disable optional_attributes@
identifier p;
type T;
@@
-T __attribute__((myattr3_mid)) p;
+__attribute__((section(".shared")))
+T p;

@disable optional_attributes@
identifier p;
type T;
@@

-T __attribute__((myattr4_mid)) p;
+T p;

@disable optional_attributes@
identifier p;
type T;
@@

-T __attribute__((myattr5_mid)) p;
+T p __attribute__((section(".shared")));
