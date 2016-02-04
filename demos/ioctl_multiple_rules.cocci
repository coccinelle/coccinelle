// can constraint to specific function by using multiple rules

@ rule1 @
struct file_operations x;
identifier ioctl_fn;
@@

x.ioctl = ioctl_fn;


@@
identifier rule1.ioctl_fn;
@@

 ioctl_fn(...) 
{

- foo(1);
+ foo(2);
}

