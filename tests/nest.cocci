@@
local function proc_info_func;
@@
  proc_info_func(...) {
    <...
-   hostno
+   hostptr->host_no
    ...>
  }


@@
local function proc_info_func;
@@
  proc_info_func(...) {
    foo();
    <...
-   xxx
+   hostptr->host_no
    ...>
  }
