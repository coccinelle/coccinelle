@@
identifier I;
identifier remove_function;
@@

struct acpi_driver I = {
  .ops = { .remove = remove_function }
};

@@
identifier f;
@@

f(...) {
  <...
(
  if (acpi_device_dir(device))
+ {
    remove_proc_entry(acpi_device_bid(device), acpi_ac_dir);
+   acpi_device_dir(device) = NULL;
+ }
|
  if (acpi_device_dir(device))
  {
    remove_proc_entry(acpi_device_bid(device), acpi_ac_dir);
+   acpi_device_dir(device) = NULL;
  }
)
  ...>
}

// exactly one layer of inter-proceduralness...
remove_function(...) {
  <...
  f(...)
  ...>
}
