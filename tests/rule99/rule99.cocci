@@
identifier I;
expression E;
@@

// the first part is in case there is already a value.  not clear from the
// git files whether this might be the case.  some with earlier dates seem
// to already have it

(
struct usb_driver I = {
 	.no_dynamic_id = 	E
};
|
struct usb_driver I = {
        ...
+	, .no_dynamic_id = 	1
};
)