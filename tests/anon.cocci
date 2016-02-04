// the case where the third argument is something else (with a type) (delayed)
// pointer type
@ is_delayed @
type local_type;
local_type *device;
expression E;
identifier fld;
@@

schedule_delayed_work(&device->fld,E)

@ rule2 @
is_delayed.local_type *device;
identifier is_delayed.fld;
@@

- schedule_work(&device->fld)
+ schedule_delayed_work(&device->fld, 0)
