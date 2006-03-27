@@
local function init_fn;
local function init_callee;
expression X, Y, Z, A, B;
identifier nr_parts1, nr_parts2;
constant XXX, YYY;
@@

(
+  static const char *probes[] = { "cmdlinepart", "RedBoot", NULL };
|
+  static const char *probes[] = { "RedBoot", "cmdlinepart", NULL };
|
+  static const char *probes[] = { "cmdlinepart", NULL };
)

  init_callee(...) {
    ooo
- #ifdef XXX
    nr_parts1 = parse_cmdline_partitions(X,Y,Z);
    nr_parts1 = parse_mtd_partitions(X,probes,Y,0);
    if (nr_parts1 > 0) {
      ...
    }
- #endif
    ooo
- #ifdef YYY
-   nr_parts2 = parse_redboot_partitions(A,B);
-   if (nr_parts2 > 0) {
-     ...
-   }
- #endif
    ooo
  }

init_fn(...) {
  ...
  init_callee(...);
  ...
}

module_init(init_fn);
