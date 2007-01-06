@@
identifier proc_info_func;
@@

struct SHT usb_stor_host_template = {
	.proc_info =			proc_info_func
};

@@
@@

proc_info_func(...) {
- f(1);
+ g(1);       
}