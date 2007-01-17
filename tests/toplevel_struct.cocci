@@
identifier proc_info_func, proc_info_func2;
@@

struct SHT usb_stor_host_template = {
	.proc_info =			proc_info_func,
	.proc_info2 =			proc_info_func2,
+	.foo = 12
};

//@@
//identifier proc_info_func, proc_info_func2;
//@@
//
//struct SHT xxx = {
//	.proc_info2 =			proc_info_func2,
//+	.proc_info =			proc_info_func,
//+	.foo = 12
//};
//@@
//identifier proc_info_func, proc_info_func2;
//@@
//
//struct SHT yyy = {
//+	.proc_info2 =			proc_info_func2,
//+ 	.proc_info =			proc_info_func,
//+	.foo = 12
//};

@@
@@

proc_info_func(...) {
- f(1);
+ g(1);       
}

@@
@@

proc_info_func2(...) {
- f(27);
+ g(27);       
}