@@
constant char [] c;
identifier f;
@@

f(...,
- c
+ "%s crtl_request : bRequestType:0x%x bRequest:0x%x Value:0x%x\n"
,...)
