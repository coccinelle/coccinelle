@@
struct acpi_driver global;
identifier device;
local function remove_func, remove_func_helper;
@@

 global.ops.remove = &remove_func;
 ooo
 remove_func(...) {
   ...
   remove_func_helper(...)
   ...
 }
 ooo
 remove_func_helper(...) {
   ...
   if(acpi_device_dir(device)) {
       remove_proc_entry(acpi_device_bid(device), acpi_ac_dir);
+      acpi_device_dir(device) = NULL;
   }
   ...
 }           


