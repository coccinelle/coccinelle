// better perhaps to use a 
// declaration MODULE_PARM; 
// declaration module_parm; 
// ?
@@
expression x,y;
@@

- MODULE_PARM(x,y);
+ module_param(x,int,y);

