// - if(copy_from_user(&x, arg))   does not work. 
// the if-isomorphism does not work :(

@@ 
type T; 
identifier x, fld; 
function ioctl; // should be caracterised by another rule
@@


 ioctl(..., void *arg, ...) {
 <...
- T x;
+ T *x = arg;
 ...
- if(copy_from_user(&x, arg)!=0) 
- { ... return ...; } 
 <...
(
- x.fld
+ x->fld
| 
- &x
+ x
)
 ...>
- if(copy_to_user(arg, &x))
- { ... return ...; }
 ...>
 }